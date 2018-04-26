{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComponentTest where

import RIO
import Control.Exception (ErrorCall(..), getMaskingState, MaskingState(..))

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit

import Control.Teardown (toredownCount)
import qualified Control.Monad.Component as SUT


tests :: TestTree
tests =
  testGroup "ComponentM"
  [
    testGroup "Async Exceptions"
    [
      testCase "it releases previously allocated resources" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          alloc = return ()
          release = const $ return ()

          componentOne =
            SUT.buildComponent "one" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentTwo =
            SUT.buildComponent "two" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentThree =
            SUT.buildComponent_ "three" (threadDelay 10100100)

          componentAction = do
            componentOne
            componentTwo
            componentThree

        resultAsync <-
          async
            $ SUT.runComponentM
              "test application"
              componentAction
              (const $ return ())

        threadDelay 500
        cancel resultAsync

        result <- waitCatch resultAsync
        case result of
          Left _err -> do
            callCount <- readIORef callCountRef
            assertEqual "teardown action got called more than once" 2 callCount

          Right _ ->
            assertFailure "expecting failure; got success instead"

    ]

  , testGroup "Sync Exceptions"
    [
      testCase "it releases previously allocated resources" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          alloc = return ()
          release = const $ return ()

          componentOne =
            SUT.buildComponent "one" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentTwo =
            SUT.buildComponent "two" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentThree =
            SUT.buildComponent "three"
              alloc
              release
              (const $ throwIO (ErrorCall "failing two"))

          componentFour =
            SUT.buildComponent_ "four" (throwIO $ ErrorCall "failing three")

          componentAction = do
            componentOne
            void componentTwo
            void componentThree
            componentFour

        result <- try $ SUT.runComponentM "test application" componentAction (const $ return ())
        case result of
          Left (SUT.ComponentBuildFailed _appErr teardownResult) -> do
            callCount <- readIORef callCountRef
            assertEqual "got more than one two valid components" 2 callCount

            -- 1. component one
            -- 2. component two
            -- 3. allocated component three (notice, error happens once allocation is done)
            assertEqual "There should be three toredown resources" 3 (toredownCount teardownResult)

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "component construction allows throwM calls" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          alloc = return ()
          release = const $ return ()

          componentOne =
            SUT.buildComponent "one" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentTwo =
            SUT.buildComponent "two" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentAction = do
            componentOne
            void $ throwM (ErrorCall "failing via MonadThrow")
            componentTwo

        result <- try $ SUT.runComponentM "test application" componentAction return
        case result of
          Left (SUT.ComponentBuildFailed [SUT.ComponentBuildFailure err] _teardownResult) -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing via MonadThrow")
                        (fromException err)
            assertEqual "teardown action got called more than once" 1 callCount

          Left err -> do
            assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    ]

  , testGroup "Masking"
    [
      testCase "app callback is _always_ on unmasked state" $ do
        maskingVar <- newEmptyMVar
        let
          alloc = return ()
          release _ = getMaskingState >>= putMVar maskingVar
          componentOne = SUT.buildComponent "one" alloc release return

        SUT.runComponentM "app" componentOne return
        masking <- takeMVar maskingVar

        assertEqual "App callback is always in unmasked state" MaskedUninterruptible masking
    ]

  , testGroup "Monad"
    [
      testCase "aggregates multiple component teardown values" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          alloc = return ()
          release = const $ return ()

          componentOne =
            SUT.buildComponent "one" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentTwo =
            SUT.buildComponent "two" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentThree =
            SUT.buildComponent_ "three" $ return ()

          componentAction = do
            componentOne
            componentTwo
            componentThree

        _result <-
          SUT.runComponentM
            "test application"
            componentAction
            (const $ return ())

        callCount <- readIORef callCountRef
        assertEqual "teardown action got called more than once" 2 callCount


    , testCase "initializes components sequentially on the same thread" $ do
        let
          componentOne =
            SUT.buildComponent_ "one" myThreadId

          componentTwo =
            SUT.buildComponent_ "two" myThreadId

          componentThree =
            SUT.buildComponent_ "three" myThreadId

          componentAction = do
            t1 <- componentOne
            t2 <- componentTwo
            t3 <- componentThree
            return (t1,t2,t3)


        (t1, t2, t3) <- SUT.runComponentM "test application" componentAction return

        assertBool "expecting t1 to be equal to t2" (t1 == t2)
        assertBool "expecting t2 to be equal to t3" (t2 == t3)
    ]

  , testGroup "Applicative"
    [
      testCase "reports multiple failures" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          alloc = return ()
          release = const $ return ()

          componentOne =
            SUT.buildComponent "one" alloc release $ \_ -> do
              modifyIORef callCountRef (+1)
              return ()

          componentTwo =
            SUT.buildComponent_ "two" $ throwIO (ErrorCall "failing two")

          componentThree =
            SUT.buildComponent "three" alloc release $ \_ -> do
              void $ throwIO (ErrorCall "failing three")
              return ()

          componentAction =
            componentOne
            *> componentTwo
            *> componentThree

        result <- try $ SUT.runComponentM "test application" componentAction return
        case result of
          Left (SUT.ComponentBuildFailed [ SUT.ComponentStartupFailure _desc2 err2
                                         , SUT.ComponentStartupFailure _desc3 err3
                                         ] _teardownResult) -> do

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing two")
                        (fromException err2)

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing three")
                        (fromException err3)

            callCount <- readIORef callCountRef
            assertEqual "teardown action got called more than once" 1 callCount

          Left err -> do
            assertFailure $ "expected ComponentStartupFailure exception with two errors, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "initializes components concurrently" $ do
        let
          componentOne =
            SUT.buildComponent_ "one" myThreadId

          componentTwo =
            SUT.buildComponent_ "two" myThreadId

          componentThree =
            SUT.buildComponent_ "three" myThreadId

          componentAction =
            (,,) <$> componentOne <*> componentTwo <*> componentThree


        (t1, t2, t3) <- SUT.runComponentM "test application" componentAction return

        assertBool "expecting t1 to be different than t2" (t1 /= t2)
        assertBool "expecting t2 to be different than t3" (t2 /= t3)
        assertBool "expecting t1 to be different than t3" (t1 /= t3)
    ]
  ]
