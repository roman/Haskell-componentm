{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Core where

import RIO
import RIO.Time (NominalDiffTime, diffUTCTime, getCurrentTime)

import Control.Monad.Component.Internal.Types
import Control.Teardown (IResource, newTeardown, runTeardown_, emptyTeardown)

--------------------------------------------------------------------------------

-- | Track duration time of the execution of an IO sub-routine
trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start  <- getCurrentTime
  result <- routine
  end    <- getCurrentTime
  return (diffUTCTime end start, result)

-- | Given the name and a `ComponentM` sub-routine, this function builds an `IO`
-- sub-routine that returns a `Component` record.
--
-- The name argument is used for trace-ability purposes when executing the
-- `teardown` of a resulting `Component`.
--
-- * A note on error scenarios:
--
-- Sometimes the given `ComponentM` sub-routine may fail on execution, in such
-- cases, this function will teardown all component resources allocated so far
-- and throw a `ComponentStartupFailure` exception.
--
runComponentM :: Text -> ComponentM a -> IO (Component a)
runComponentM !appName (ComponentM ma) = do
  eResult <- ma
  case eResult of
    Left (errList, componentTree) -> do
      teardownList <- componentTreeToTeardown componentTree
      appTeardown <- newTeardown appName teardownList
      -- Cleanup resources allocated so far and throw error
      -- list
      runTeardown_ appTeardown
      throwIO (ComponentStartupFailure errList)

    Right (a, componentTree) -> do
      teardownList <- componentTreeToTeardown componentTree
      appTeardown  <- newTeardown appName teardownList
      return $! Component a componentTree appTeardown

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine must return a tuple where:
--
-- * First position represents the resource being returned from
--   the component
--
-- * Second position represents a cleanup operation that tears down allocated
--   resources to create the first element of the tuple
--
buildComponent :: IResource cleanup => Text ->  IO (a, cleanup) -> ComponentM a
buildComponent !desc !ma =
  ComponentM $ do
    (elapsedTime, (a, cleanupAction)) <- trackExecutionTime ma
    teardownAction <- newTeardown desc cleanupAction
    return $ Right (a, ComponentBranch desc elapsedTime teardownAction [])

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine returns a resource that does not allocate any other
-- resources that would need to be cleaned up on a system shutdown.
--
buildComponent_ :: Text -> IO a -> ComponentM a
buildComponent_ !desc !ma =
  ComponentM $ do
    (elapsedTime, a) <- trackExecutionTime ma
    return $ Right (a, ComponentBranch desc elapsedTime (emptyTeardown desc) [])
