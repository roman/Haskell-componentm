{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import RIO
import RIO.Time (NominalDiffTime)
import qualified RIO.Set as S
import qualified RIO.HashMap as M.Hash
import qualified RIO.Text as Text

import           Data.Dynamic (Dynamic, toDyn, fromDynamic)
import           Data.Graph (graphFromEdges', topSort)
import           Control.Monad.Catch    (MonadThrow (..))

import Control.Teardown (HasTeardown (..), Teardown)

--------------------------------------------------------------------------------

data ComponentError
  = ComponentFailure !Text
  | ComponentStartupFailure ![SomeException]
  deriving (Generic, Show)

instance Exception ComponentError

type Description = Text

data ComponentNode
  = ComponentNode {
      cnValue       :: !(Async Dynamic)
    , cnElapsedTime :: !NominalDiffTime
    , cnDependencies:: !(Set Description)
    , cnTeardown    :: !Teardown
    }
  deriving (Generic)

type DependencyTable =
  HashMap Description Dynamic -- ComponentNode

-- appendDependency :: Description -> ComponentNode -> ComponentNode
-- appendDependency dependency node =
--   node {
--     cnDependencies =
--       S.insert dependency (cnDependencies node)
--   }

-- joinDependencyTables :: DependencyTable -> DependencyTable -> DependencyTable
-- joinDependencyTables depTable1 depTable2 =
--   let
--     appendDependenciesToNode node =
--       foldr appendDependency node (M.Hash.keys depTable1)
--   in
--     -- First, we add all keys from depTable1 to all nodes of depTable2
--     M.Hash.map appendDependenciesToNode depTable2
--     -- Then, we join both depTable1 and depTable2
--     & M.Hash.union depTable1

-- dependencyTableToTeardown :: DependencyTable -> [Teardown]
-- dependencyTableToTeardown depTable =
--   let
--     componentGraphEdges :: [(ComponentNode, Description, [Description])]
--     componentGraphEdges =
--       M.Hash.foldrWithKey
--         (\k node acc -> (node, k, S.toList $ cnDependencies node):acc)
--         []
--         depTable

--     (componentGraph, lookupComponentNode) =
--       graphFromEdges' componentGraphEdges

--   in
--     map (\node ->
--            let (component, _, _) = lookupComponentNode node
--            in cnTeardown component)
--         (topSort componentGraph)

-- | `ComponentM` is a wrapper of the `IO` monad that automatically deals with
-- the composition of `Teardown` sub-routines from resources allocated in every
-- resource of your application. To build `ComponentM` actions see the
-- `buildComponent`, `buildComponentWithCleanup` and
-- `buildComponentWithTeardown` functions.
newtype ComponentM a
  = ComponentM (IO (Either ([SomeException], DependencyTable)
                           (IO a, Maybe Description, TVar DependencyTable)))

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, desc, depTableVar) ->
          Right (f <$> a, desc, depTableVar)


depTableUnion
  :: TVar DependencyTable
  -> TVar DependencyTable
  -> STM (TVar DependencyTable, DependencyTable)
depTableUnion depTableVarA depTableVarB = do
  depTableVar <- newTVar M.Hash.empty
  depTable <- M.Hash.union <$> readTVar depTableVarA <*> readTVar depTableVarB
  writeTVar depTableVar depTable
  return (depTableVar, depTable)

depTableUnionIO
  :: TVar DependencyTable
  -> TVar DependencyTable
  -> IO (TVar DependencyTable, DependencyTable)
depTableUnionIO depTableVarA depTableVarB =
  atomically $ depTableUnion depTableVarA depTableVarB

readCacheOrBuild
  :: (Eq k, Hashable k, Typeable a)
  => k
  -> IO a
  -> HashMap k Dynamic
  -> IO (Async a, HashMap k Dynamic)
readCacheOrBuild key ctor cache = do
  case M.Hash.lookup key cache of
    Nothing -> do
      valueAsync <- async ctor
      return (valueAsync, M.Hash.insert key (toDyn valueAsync) cache)
    Just dynAsync -> do
      -- Dangerous, but we know what we are doing ;-)
      let valueAsync = fromMaybe undefined $ fromDynamic dynAsync
      return (valueAsync, cache)

instance Applicative ComponentM where
  pure a =
    ComponentM $ do
      depTableVar <- newTVarIO M.Hash.empty
      return $ Right (return a, Nothing, depTableVar)

  (ComponentM f0) <*> (ComponentM a0) = ComponentM $ do
    eF <- try f0
    eA <- try a0
    case (eF, eA) of
      -- both ComponentM IO throw exceptions
      ( Left errF, Left errA ) ->
        return $ Left ( [errF, errA], M.Hash.empty)

      -- @f@ ComponentM IO throw exception
      -- @a@ user IO throw exception
      ( Left errF, Right (Left (errA, depTableA)) ) ->
        return $ Left ( [errF] <> errA, depTableA )

      -- @f@ ComponentM IO throw exception
      -- @a@ happy path
      ( Left errF, Right (Right (_, _, depTableVarA)) ) -> do
        depTableA <- atomically $ readTVar depTableVarA
        return $ Left ( [errF], depTableA )

      -- @f@ user IO throw exception
      -- @a@ ComponentM IO throw exception
      ( Right (Left (errF, depTableVarF)), Left errA ) ->
        return $ Left ( errF <> [errA], depTableVarF )

      -- @f@ happy path
      -- @a@ ComponentM IO throw exception
      ( Right (Right (_, _, depTableVarF)), Left errA ) -> do
        depTableF <- atomically $ readTVar depTableVarF
        return $ Left ( [errA], depTableF )

      -- @f@ user IO throw exception
      -- @a@ happy path
      ( Right (Left (errF, depTableF)), Right (Right (_, _, depTableRefA)) ) -> do
        depTableA <- atomically $ readTVar depTableRefA
        return $ Left ( errF
                      , depTableF <> depTableA
                      )

      -- @f@ user IO throw exception
      -- @a@ user IO throw exception
      ( Right (Left (errF, depTableF)), Right (Left (errA, depTableA)) ) ->
        return $ Left ( errF <> errA
                      , depTableF <> depTableA
                      )


      -- @f@ happy path
      -- @a@ user IO throws exception
      ( Right (Right (_, _, depTableVarF)), Right (Left (errA, depTableA)) ) -> do
        depTableF <- atomically $ readTVar depTableVarF
        return $ Left ( errA
                      , depTableF <> depTableA
                      )

      -- @f@ happy path
      -- @a@ happy path
      ( Right ( Right (mkValF, mdescF, depTableVarF) ), Right ( Right (mkValA, mdescA, depTableVarA) ) ) -> do
        (depTableVar, depTable) <- depTableUnionIO depTableVarF depTableVarA

        case (mdescF, mdescA) of
          (Nothing, Nothing) ->
            return $ Right $ (mkValF <*> mkValA, Nothing, depTableVar)

          (Just descF, Nothing) -> do
            (asyncF, depTable1) <- readCacheOrBuild descF mkValF depTable
            valA <- mkValA
            atomically $ writeTVar depTableVar depTable1
            eValF <- waitCatch asyncF
            case eValF of
              Left _ -> undefined
              Right valF ->
                return $ Right (return $ valF valA, Nothing, depTableVar)

          (Nothing, Just descA) -> do
            valF <- mkValF
            (asyncA, depTable1) <- readCacheOrBuild descA mkValF depTable
            atomically $ writeTVar depTableVar depTable1
            eValA <- waitCatch asyncA
            case eValA of
              Left _ -> undefined
              Right valA ->
                return $ Right (return $ valF valA, Nothing, depTableVar)


          (Just descF, Just descA) -> do
            (asyncF, depTable1) <- readCacheOrBuild descF mkValF depTable
            (asyncA, depTable2) <- readCacheOrBuild descA mkValA depTable1

            atomically $ writeTVar depTableVar depTable2

            eresult <- waitCatch $ asyncF <*> asyncA

            case eresult of
              Left _ -> undefined
              Right result ->
                return $ Right (return result, Nothing, depTableVar)

        -- case result of
        --   Left err ->
        --   Right res ->

        -- return $ Right ( fVal aVal
        --                , fDepTable <> aDepTable
        --                )


    -- (createf, descF, depTableFVar) <- f0
    -- (createA, descA, depTableAVar) <- a0

    -- depTableVar <- atomically $ do
    --   depTable <- M.Hash.union <$> (readTVar depTableFVar) <*> (readTVar depTableAVar)
    --   return depTable
    -- undefined

--     withAsync f $ \fAsync ->
--       withAsync a $ \aAsync -> do
--         fValue <- waitCatch fAsync
--         aValue <- waitCatch aAsync
--         case (fValue, aValue) of




-- instance Monad ComponentM where
--   return =
--     pure

--   (ComponentM m) >>= f = ComponentM $ do
--     aResult <- m
--     case aResult of
--       Right (a, aDepTable) -> do
--         let
--           (ComponentM m1) = f a

--         bResult <- try m1

--         case bResult of
--           -- There was an exception via the IO Monad
--           Left bErr ->
--             return $ Left ([bErr], aDepTable)

--           -- There was an exception either via `fail` or `throwM`
--           Right (Left (bErr, bDepTable)) ->
--             return
--             $ Left (bErr, joinDependencyTables aDepTable bDepTable)

--           Right (Right (b, bDepTable)) ->
--             return
--             $ Right (b, joinDependencyTables aDepTable bDepTable)


--       Left (aErr, aDepTable) ->
--         return $ Left (aErr, aDepTable)

--   fail str =
--     ComponentM
--       $ return
--       $ Left ([toException $! ComponentFailure (Text.pack str)], M.Hash.empty)

-- instance MonadThrow ComponentM where
--   throwM e =
--     ComponentM
--       $ return
--       $ Left ([toException e], M.Hash.empty)

-- instance MonadIO ComponentM where
--   liftIO action = ComponentM $ do
--     result <- action
--     return $ Right (result, M.Hash.empty)


-- -- | Represents the result of a `ComponentM` sub-routine, it contains a resource
-- -- record which can be recovered using `fromComponent` and a `Teardown`
-- -- sub-routine that can be executed using the `teardown` function.
-- data Component a
--   = Component { componentResource :: !a
--               , componentTeardown :: !Teardown }
--   deriving (Generic)

-- -- | Fetches the resource of a `Component` returned by a `ComponentM`
-- -- sub-routine.
-- fromComponent :: Component a -> a
-- fromComponent =
--   componentResource
-- {-# INLINE fromComponent #-}

-- instance NFData a => NFData (Component a)

-- instance HasTeardown (Component a) where
--   getTeardown =
--     componentTeardown
