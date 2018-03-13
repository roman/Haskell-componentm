{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Core
  (
    withComponentM
  , withComponentM2
  , buildComponent
  , buildComponent_
  ) where

import RIO
import RIO.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import RIO.List (unzip)
import qualified RIO.HashMap as M.Hash

import Control.Monad.Operational (ProgramViewT(..), singleton, viewT)
import Control.Teardown (IResource, TeardownResult, newTeardown, runTeardown, emptyTeardown)
import Data.Dynamic (fromDynamic, toDyn)

import Control.Monad.Component.Internal.Types

--------------------------------------------------------------------------------

-- | Track duration time of the execution of an IO sub-routine
trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start  <- getCurrentTime
  result <- routine
  end    <- getCurrentTime
  return (diffUTCTime end start, result)

-- | Enhances @withComponentM@ with an extra callback arugment that will receive
-- a @TeardownResult@ record. This callback function can be used to render what
-- components were cleaned up, if any of them failed and how much time they took
-- to process it.
--
withComponentM2
  :: Typeable a
  => Text
  -> ComponentM a
  -> ((a, [BuildInfo]) -> IO ())
  -> (TeardownResult -> IO ())
  -> IO ()
withComponentM2 !appName !(ComponentM buildComponentI) !runApp !renderTeardownResult = do
    bracket
      (do (result, resourcesList) <- eval0 M.Hash.empty [] buildComponentI
          let (buildInfoList, teardownList) = unzip resourcesList
          appTeardown <- newTeardown appName teardownList
          return ((result, reverse buildInfoList) , appTeardown))
      (runTeardown1 . snd)
      (runApp . fst)
  where
    runTeardown1 teardown = do
      teardownResult <- runTeardown teardown
      renderTeardownResult teardownResult

    eval0 cacheTable resourcesList instr = try (viewT instr) >>= eval cacheTable resourcesList
    eval cacheTable resourcesList einstr =
      case einstr of
        Left err -> do
          appTeardown <- newTeardown appName (map snd resourcesList)
          runTeardown1 appTeardown
          throwIO (ComponentBuildFailure (err :: SomeException))

        Right (Return a) ->
          return (a, resourcesList)

        Right (FailBuilder err :>>= _) -> do
          appTeardown <- newTeardown appName (map snd resourcesList)
          runTeardown1 appTeardown
          throwIO err

        Right (BuildComponent componentKey ctor :>>= next) ->
          case M.Hash.lookup componentKey cacheTable of
            Nothing -> mask $ \unmask -> do
              (time, eresult) <- trackExecutionTime $ unmask $ try ctor
              case eresult of
                Left err -> do
                  let (buildInfoList0, teardownList) = unzip resourcesList
                      buildInfoList = BuildInfo componentKey time : buildInfoList0

                  appTeardown <- newTeardown appName teardownList
                  runTeardown1 appTeardown
                  throwIO (ComponentInitFailure err (reverse buildInfoList))

                Right (result, teardown) -> do
                  let
                    newBuild = toDyn result
                    cacheTable1 = M.Hash.insert componentKey newBuild cacheTable
                    resourcesList1 = (BuildInfo componentKey time, teardown) : resourcesList
                  unmask $ eval0 cacheTable1 resourcesList1 (next result)

            Just prevResult -> do
              let
                oldBuild = fromMaybe (error "won't happen") $ fromDynamic prevResult
              eval0 cacheTable resourcesList (next oldBuild)

-- | Given an application name* and a `ComponentM` sub-routine, this function builds a
-- component and calls a sub-routine that executes your main application.
--
-- (*) The @name@ argument is used for tracing purposes.
--
-- * A note on error scenarios:
--
-- Whenever a given @ComponentM@ initialization sub-routine fails on execution,
-- this function will automatically teardown all component resources allocated
-- so far and throw a `ComponentInitFailure` exception.
--
withComponentM :: Typeable a => Text -> ComponentM a -> ((a, [BuildInfo]) -> IO ()) -> IO ()
withComponentM appName (ComponentM buildComponentI) runApp =
  withComponentM2 appName (ComponentM buildComponentI) runApp (const $ return ())

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine must return a tuple where:
--
-- * First position represents the resource being returned from
--   the component
--
-- * Second position represents a cleanup operation that tears down allocated
--   resources to create the first element of the tuple
--
buildComponent
  :: (Typeable a, IResource cleanup)
  => ComponentKey -> IO (a, cleanup) -> ComponentM a
buildComponent componentKey builder =
  let
    builder1 = do
      (a, cleanup) <- builder
      teardown <- newTeardown componentKey cleanup
      return (a, teardown)
  in
    ComponentM $ singleton $ BuildComponent componentKey builder1

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine returns a resource that does not allocate any other
-- resources that would need to be cleaned up on a system shutdown.
--
buildComponent_ :: Typeable a => ComponentKey -> IO a -> ComponentM a
buildComponent_ componentKey builder =
  let
    builder1 = fmap (\a -> (a, emptyTeardown componentKey)) builder
  in
    ComponentM $ singleton $ BuildComponent componentKey builder1
