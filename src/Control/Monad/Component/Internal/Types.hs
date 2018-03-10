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
import           Control.Monad.Fail     (MonadFail (..))

import Control.Teardown (HasTeardown (..), Teardown)

--------------------------------------------------------------------------------

data ComponentError
  = ComponentFailure !Text
  | ComponentStartupFailure ![SomeException]
  deriving (Generic, Show)

instance Exception ComponentError

type Description = Text

data ComponentItem
  = ComponentItem {
      ciDescription            :: !Description
    , ciElapsedTime :: !NominalDiffTime
    , ciDependencies    :: !(Set Description)
    , ciTeardown    :: !Teardown
    }
  deriving (Generic)

type ComponentDeps =
  HashMap Description ComponentItem

appendDependency :: Description -> ComponentItem -> ComponentItem
appendDependency dependency component =
  component {
    ciDependencies =
      S.insert dependency (ciDependencies component)
  }

appendDependencies
  :: ComponentDeps
  -> ComponentDeps
  -> ComponentDeps
appendDependencies deps current =
  M.Hash.map (\dep -> foldr appendDependency dep (M.Hash.keys deps)) current
  & M.Hash.union deps

componentToTeardown :: ComponentDeps -> [Teardown]
componentToTeardown depMap =
  let
    depList :: [(ComponentItem, Description, [Description])]
    depList =
      M.Hash.foldrWithKey
        (\k node acc -> (node, k, S.toList $ ciDependencies node):acc)
        []
        depMap

    (componentGraph, lookupComponent) =
      graphFromEdges' depList

  in
    map (\vertex ->
           let (component, _, _) = lookupComponent vertex
           in ciTeardown component)
        (topSort componentGraph)

-- | `ComponentM` is a wrapper of the `IO` monad that automatically deals with
-- the composition of `Teardown` sub-routines from resources allocated in every
-- resource of your application. To build `ComponentM` actions see the
-- `buildComponent`, `buildComponentWithCleanup` and
-- `buildComponentWithTeardown` functions.
newtype ComponentM a
  = ComponentM (IO (Either ([SomeException], ComponentDeps)
                           (a, ComponentDeps)))

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

  (ComponentM mf) <*> (ComponentM mm) = ComponentM $ do
    withAsync mf $ \amf ->
      withAsync mm $ \amm -> do
        ef <- waitCatch amf
        em <- waitCatch amm
        case (ef, em) of
          ( Left err1, Left err2 ) ->
            return $ Left ( [err1, err2], M.Hash.empty )

          ( Left err1, Right (Left (err2, cs2)) ) ->
            return $ Left ( [err1] <> err2, cs2 )

          ( Left err1, Right (Right (_, cs2)) ) ->
            return $ Left ( [err1], cs2 )


          ( Right (Left (err1, deps1)), Left err2 ) ->
            return $ Left ( err1 <> [err2], deps1 )

          ( Right (Right (_, deps1)), Left err2 ) ->
            return $ Left ( [err2], deps1 )

          ( Right (Left (err, deps1)), Right (Right (_, cs2)) ) ->
            return $ Left ( err
                          , deps1 <> cs2
                          )

          ( Right (Left (err1, deps1)), Right (Left (err2, cs2)) ) ->
            return $ Left ( err1 <> err2
                          , deps1 <> cs2
                          )

          ( Right (Right (_, deps1)), Right (Left (err, cs2)) ) ->
            return $ Left ( err
                          , deps1 <> cs2
                          )

          ( Right (Right (f, deps1)), Right (Right (a, cs2)) ) ->
            return $ Right ( f a
                           , deps1 <> cs2
                           )

instance Monad ComponentM where
  return =
    pure

  (ComponentM action0) >>= f = ComponentM $ do
    eResult0 <- action0
    case eResult0 of
      Right (a, deps0) -> do
        let
          (ComponentM action1) = f a

        eResult1 <- try action1

        case eResult1 of
          -- There was an exception via the IO Monad
          Left err ->
            return $ Left ([err], deps0)

          -- There was an exception either via `fail` or `throwM`
          Right (Left (err, deps1)) ->
            return
            $ Left (err, appendDependencies deps0 deps1)

          Right (Right (b, deps1)) ->
            return
            $ Right (b, appendDependencies deps0 deps1)


      Left (err, deps0) ->
        return $ Left (err, deps0)

instance MonadFail ComponentM where
  fail str =
    ComponentM
      $ return
      $ Left ([toException $! ComponentFailure (Text.pack str)], M.Hash.empty)

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
