{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Types where

import RIO

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.Fail     (MonadFail (..))
import qualified RIO.Text              as T

import Data.Time (NominalDiffTime)

import Control.Teardown (HasTeardown (..), Teardown, newTeardown)

--------------------------------------------------------------------------------

data ComponentError
  = ComponentFailure !Text
  | ComponentStartupFailure ![SomeException]
  deriving (Generic, Show)

instance Exception ComponentError

data ComponentNode
  = ComponentBranch
    {
      componentNodeDesc         :: !Text
    , componentNodeInitTime     :: !NominalDiffTime
    , componentNodeTeardown     :: !Teardown
    , componentNodeDependencies :: ![ComponentNode]
    }
  | ComponentSiblings { componentNodeSiblings :: ![ComponentNode] }
  | ComponentLeaf
    deriving (Generic)

type ComponentTree = ComponentNode

addDependency :: ComponentNode -> ComponentNode -> ComponentNode
addDependency dependency component =
  case (dependency, component) of
    (ComponentLeaf, _) -> component
    (_, ComponentLeaf) -> dependency
    (_, ComponentSiblings {componentNodeSiblings}) ->
      ComponentSiblings (map (addDependency dependency) componentNodeSiblings)
    (_, ComponentBranch {componentNodeDependencies = dependencies,..}) ->
      ComponentBranch {
          componentNodeDependencies = dependency:dependencies
        , ..
        }

makeSiblings :: ComponentNode -> ComponentNode -> ComponentNode
makeSiblings comp1 comp2 =
  case (comp1, comp2) of
    (ComponentLeaf, _) -> comp2
    (_, ComponentLeaf) -> comp1
    (ComponentSiblings compList1, ComponentSiblings compList2) ->
      ComponentSiblings $ compList1 <> compList2
    (ComponentBranch {}, ComponentBranch {}) ->
      ComponentSiblings [comp1, comp2]
    (ComponentSiblings compList, ComponentBranch {}) ->
      ComponentSiblings (comp2:compList)
    (ComponentBranch {}, ComponentSiblings compList) ->
      ComponentSiblings (comp1:compList)

componentTreeToTeardown :: ComponentTree -> IO [Teardown]
componentTreeToTeardown component =
  case component of
    ComponentLeaf -> return []
    ComponentSiblings {componentNodeSiblings} -> do
      siblingTeardowns <- mapM componentTreeToTeardown componentNodeSiblings
      return $ concat siblingTeardowns
    ComponentBranch {componentNodeDesc, componentNodeTeardown, componentNodeDependencies} -> do
      branchTeardown <- newTeardown componentNodeDesc $ do
        depTeardowns <- mapM componentTreeToTeardown componentNodeDependencies
        return $ [componentNodeTeardown] <> concat depTeardowns
      return [branchTeardown]

instance NFData ComponentNode

-- | `ComponentM` is a wrapper of the `IO` monad that automatically deals with
-- the composition of `Teardown` sub-routines from resources allocated in every
-- resource of your application. To build `ComponentM` actions see the
-- `buildComponent`, `buildComponentWithCleanup` and
-- `buildComponentWithTeardown` functions.
newtype ComponentM a
  = ComponentM (IO (Either ([SomeException], ComponentTree)
                           (a, ComponentTree)))

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, componentNode) ->
          Right (f a, componentNode)

instance Applicative ComponentM where
  pure a =
    ComponentM
      $ return
      $ Right (a, ComponentLeaf)

  (ComponentM mf) <*> (ComponentM mm) = ComponentM $ do
    cf <- try mf
    cm <- try mm
    case (cf, cm) of
      ( Left err1, Left err2 ) ->
        return $ Left ( [err1, err2], ComponentLeaf )

      ( Left err1, Right (Left (err2, componentNode2)) ) ->
        return $ Left ( [err1] <> err2, componentNode2 )

      ( Left err1, Right (Right (_, componentNode2)) ) ->
        return $ Left ( [err1], componentNode2 )


      ( Right (Left (err1, componentNode1)), Left err2 ) ->
        return $ Left ( err1 <> [err2], componentNode1 )

      ( Right (Right (_, componentNode1)), Left err2 ) ->
        return $ Left ( [err2], componentNode1 )

      ( Right (Left (err, componentNode1)), Right (Right (_, componentNode2)) ) ->
        return $ Left ( err
                      , makeSiblings componentNode1 componentNode2
                      )

      ( Right (Left (err1, componentNode1)), Right (Left (err2, componentNode2)) ) ->
        return $ Left ( err1 <> err2
                      , makeSiblings componentNode1 componentNode2
                      )

      ( Right (Right (_, componentNode1)), Right (Left (err, componentNode2)) ) ->
        return $ Left ( err
                      , makeSiblings componentNode1 componentNode2
                      )

      ( Right (Right (f, componentNode1)), Right (Right (a, componentNode2)) ) ->
        return $ Right ( f a
                       , makeSiblings componentNode1 componentNode2
                       )

instance Monad ComponentM where
  return =
    pure

  (ComponentM action0) >>= f = ComponentM $ do
    eResult0 <- action0
    case eResult0 of
      Right (a, componentNode0) -> do
        let
          (ComponentM action1) = f a

        eResult1 <- try action1

        case eResult1 of
          -- There was an exception via the IO Monad
          Left err ->
            return $ Left ([err], componentNode0)

          -- There was an exception either via `fail` or `throwM`
          Right (Left (err, componentNode1)) ->
            return $ Left (err, addDependency componentNode0 componentNode1)

          Right (Right (b, componentNode1)) ->
            return $ Right (b, addDependency componentNode0 componentNode1)


      Left (err, cs0) ->
        return $ Left (err, cs0)

instance MonadFail ComponentM where
  fail str =
    ComponentM
      $ return
      $ Left ([toException $! ComponentFailure (T.pack str)], ComponentLeaf)

instance MonadThrow ComponentM where
  throwM e =
    ComponentM
      $ return
      $ Left ([toException e], ComponentLeaf)

instance MonadIO ComponentM where
  liftIO action = ComponentM $ do
    result <- action
    return $ Right (result, ComponentLeaf)


-- | Represents the result of a `ComponentM` sub-routine, it contains a resource
-- record which can be recovered using `fromComponent` and a `Teardown`
-- sub-routine that can be executed using the `teardown` function.
data Component a
  = Component { componentResource :: !a
              , componentEntries  :: !ComponentTree
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
