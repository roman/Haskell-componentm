{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import RIO
import RIO.Time (NominalDiffTime)
import qualified RIO.Set as S
import qualified RIO.HashMap as M.Hash
import qualified RIO.Text as Text

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
      cnDescription            :: !Description
    , cnElapsedTime :: !NominalDiffTime
    , cnDependencies    :: !(Set Description)
    , cnTeardown    :: !Teardown
    }
  deriving (Generic)

type DependencyTable =
  HashMap Description ComponentNode

appendDependency :: Description -> ComponentNode -> ComponentNode
appendDependency dependency node =
  node {
    cnDependencies =
      S.insert dependency (cnDependencies node)
  }

joinDependencyTables :: DependencyTable -> DependencyTable -> DependencyTable
joinDependencyTables depTable1 depTable2 =
  let
    appendDependenciesToNode node =
      foldr appendDependency node (M.Hash.keys depTable1)
  in
    -- First, we add all keys from depTable1 to all nodes of depTable2
    M.Hash.map appendDependenciesToNode depTable2
    -- Then, we join both depTable1 and depTable2
    & M.Hash.union depTable1

dependencyTableToTeardown :: DependencyTable -> [Teardown]
dependencyTableToTeardown depTable =
  let
    componentGraphEdges :: [(ComponentNode, Description, [Description])]
    componentGraphEdges =
      M.Hash.foldrWithKey
        (\k node acc -> (node, k, S.toList $ cnDependencies node):acc)
        []
        depTable

    (componentGraph, lookupComponentNode) =
      graphFromEdges' componentGraphEdges

  in
    map (\node ->
           let (component, _, _) = lookupComponentNode node
           in cnTeardown component)
        (topSort componentGraph)

-- | `ComponentM` is a wrapper of the `IO` monad that automatically deals with
-- the composition of `Teardown` sub-routines from resources allocated in every
-- resource of your application. To build `ComponentM` actions see the
-- `buildComponent`, `buildComponentWithCleanup` and
-- `buildComponentWithTeardown` functions.
newtype ComponentM a
  = ComponentM (IO (Either ([SomeException], DependencyTable)
                           (a, DependencyTable)))

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, deps) ->
          Right (f a, deps)

instance Applicative ComponentM where
  pure a =
    ComponentM
      $ return
      $ Right (a, M.Hash.empty)

  (ComponentM f) <*> (ComponentM a) = ComponentM $ do
    withAsync f $ \fAsync ->
      withAsync a $ \aAsync -> do
        fValue <- waitCatch fAsync
        aValue <- waitCatch aAsync
        case (fValue, aValue) of
          ( Left fErr, Left aErr ) ->
            return $ Left ( [fErr, aErr], M.Hash.empty )

          ( Left fErr, Right (Left (aErr, aDepTable)) ) ->
            return $ Left ( [fErr] <> aErr, aDepTable )

          ( Left fErr, Right (Right (_, aDepTable)) ) ->
            return $ Left ( [fErr], aDepTable )


          ( Right (Left (fErr, fDepTable)), Left aErr ) ->
            return $ Left ( fErr <> [aErr], fDepTable )

          ( Right (Right (_, fDepTable)), Left aErr ) ->
            return $ Left ( [aErr], fDepTable )

          ( Right (Left (fErr, fDepTable)), Right (Right (_, aDepTable)) ) ->
            return $ Left ( fErr
                          , fDepTable <> aDepTable
                          )

          ( Right (Left (fErr, fDepTable)), Right (Left (aErr, aDepTable)) ) ->
            return $ Left ( fErr <> aErr
                          , fDepTable <> aDepTable
                          )

          ( Right (Right (_, fDepTable)), Right (Left (aErr, aDepTable)) ) ->
            return $ Left ( aErr
                          , fDepTable <> aDepTable
                          )

          ( Right (Right (fVal, fDepTable)), Right (Right (aVal, aDepTable)) ) ->
            return $ Right ( fVal aVal
                           , fDepTable <> aDepTable
                           )

instance Monad ComponentM where
  return =
    pure

  (ComponentM m) >>= f = ComponentM $ do
    aResult <- m
    case aResult of
      Right (a, aDepTable) -> do
        let
          (ComponentM m1) = f a

        bResult <- try m1

        case bResult of
          -- There was an exception via the IO Monad
          Left bErr ->
            return $ Left ([bErr], aDepTable)

          -- There was an exception either via `fail` or `throwM`
          Right (Left (bErr, bDepTable)) ->
            return
            $ Left (bErr, joinDependencyTables aDepTable bDepTable)

          Right (Right (b, bDepTable)) ->
            return
            $ Right (b, joinDependencyTables aDepTable bDepTable)


      Left (aErr, aDepTable) ->
        return $ Left (aErr, aDepTable)

  fail str =
    ComponentM
      $ return
      $ Left ([toException $! ComponentFailure (Text.pack str)], M.Hash.empty)

-- instance MonadFail ComponentM where
--   fail str =
--     ComponentM
--       $ return
--       $ Left ([toException $! ComponentFailure (Text.pack str)], M.Hash.empty)

instance MonadThrow ComponentM where
  throwM e =
    ComponentM
      $ return
      $ Left ([toException e], M.Hash.empty)

instance MonadIO ComponentM where
  liftIO action = ComponentM $ do
    result <- action
    return $ Right (result, M.Hash.empty)


-- | Represents the result of a `ComponentM` sub-routine, it contains a resource
-- record which can be recovered using `fromComponent` and a `Teardown`
-- sub-routine that can be executed using the `teardown` function.
data Component a
  = Component { componentResource :: !a
              , componentTeardown :: !Teardown }
  deriving (Generic)

-- | Fetches the resource of a `Component` returned by a `ComponentM`
-- sub-routine.
fromComponent :: Component a -> a
fromComponent =
  componentResource
{-# INLINE fromComponent #-}

instance NFData a => NFData (Component a)

instance HasTeardown (Component a) where
  getTeardown =
    componentTeardown
