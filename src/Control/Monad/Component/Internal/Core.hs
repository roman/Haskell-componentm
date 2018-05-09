{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Component.Internal.Core where

import           RIO
import           RIO.Time                               (NominalDiffTime,
                                                         diffUTCTime,
                                                         getCurrentTime)

import qualified RIO.HashMap                            as HashMap
import qualified RIO.Set                                as Set

import           Control.Monad.Component.Internal.Types
import           Control.Teardown                       (emptyTeardown,
                                                         newTeardown,
                                                         runTeardown)

--------------------------------------------------------------------------------

-- | Track duration time of the execution of an IO sub-routine
trackExecutionTime :: IO a -> IO (NominalDiffTime, a)
trackExecutionTime routine = do
  start  <- getCurrentTime
  result <- routine
  end    <- getCurrentTime
  return (diffUTCTime end start, result)

-- | Transforms an `IO` sub-routine into a `ComponentM` sub-routine; the given
-- `IO` sub-routine returns a resource that does not allocate any other
-- resources that would need to be cleaned up on a system shutdown.
--
buildComponent_ :: Text -> IO a -> ComponentM a
buildComponent_ !componentDesc !ma = ComponentM $ mask $ \restore -> do
  (buildElapsedTime, result) <- trackExecutionTime (try $ restore ma)
  case result of
    Left err -> do
      let build = Build
            { componentDesc
            , componentTeardown = emptyTeardown componentDesc
            , buildElapsedTime
            , buildFailure      = Just err
            , buildDependencies = Set.empty
            }
          buildTable = HashMap.singleton componentDesc build
      return $ Left ([ComponentAllocationFailed componentDesc err], buildTable)

    Right output -> do
      let build = Build
            { componentDesc
            , componentTeardown = emptyTeardown componentDesc
            , buildElapsedTime
            , buildFailure      = Nothing
            , buildDependencies = Set.empty
            }
          buildTable = HashMap.singleton componentDesc build
      return $ Right (output, buildTable)

buildComponent :: Text -> IO a -> (a -> IO ()) -> (a -> IO b) -> ComponentM b
buildComponent !componentDesc !construct !release !transform =
  ComponentM $ mask $ \restore -> do

    (buildElapsedTime, (result, componentTeardown)) <- trackExecutionTime
      $ startComponent restore

    let buildFailure = either (Just . toException) (const Nothing) result

        build        = Build
          { componentDesc
          , componentTeardown
          , buildElapsedTime
          , buildFailure
          , buildDependencies = Set.empty
          }

        buildTable = HashMap.singleton componentDesc build

    case result of
      Left  err    -> return $ Left ([err], buildTable)

      Right output -> return $ Right (output, buildTable)
 where
  startComponent restore = do
    resource         <- construct
    resourceTeardown <- newTeardown componentDesc (release resource)
    result           <- restore $ try $ transform resource
    return $ case result of
      Left err ->
        (Left $ ComponentAllocationFailed componentDesc err, resourceTeardown)
      Right output -> (Right output, resourceTeardown)

runComponentM1
  :: (ComponentEvent -> IO ()) -> Text -> ComponentM a -> (a -> IO b) -> IO b
runComponentM1 !logFn !appName (ComponentM buildFn) !appFn =
  mask $ \restore -> do
    result <- restore buildFn
    case result of
      Left (errList, buildTable) -> do
        appTeardown    <- buildTableToTeardown appName buildTable
        teardownResult <- runTeardown appTeardown
        restore $ logFn $ ComponentReleased teardownResult
        throwIO $ ComponentBuildFailed errList teardownResult

      Right (resource, buildTable) -> do
        let buildList = buildTableToOrderedList buildTable
        restore $ logFn $ ComponentBuilt $ BuildResult $ reverse buildList

        appTeardown    <- buildTableToTeardown appName buildTable
        appResult      <- tryAny $ restore $ appFn resource
        teardownResult <- runTeardown appTeardown
        restore $ logFn $ ComponentReleased teardownResult

        case appResult of
          Left appError ->
            throwIO $ ComponentRuntimeFailed appError teardownResult
          Right output -> return output

runComponentM :: Text -> ComponentM a -> (a -> IO b) -> IO b
runComponentM = runComponentM1 (const $ return ())
