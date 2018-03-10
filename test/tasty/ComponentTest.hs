{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ComponentTest where

import RIO
import Control.Exception (ErrorCall(..))
import Control.Concurrent (myThreadId)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit

import Control.Teardown (toredownCount)

import qualified Control.Monad.Component as SUT


tests :: TestTree
tests =
  testGroup "ComponentM"
  [
    testGroup "Monad"
    [
      testCase "aggregates multiple component teardown values" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne =
            SUT.buildComponent "one" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentTwo =
            SUT.buildComponent "two" $ do
              t <- SUT.newTeardown "internal" (modifyIORef callCountRef (+1) :: IO ())
              return ((), t)

          componentThree =
            SUT.buildComponent_ "three" $ return ()

          componentAction = do
            componentOne
            componentTwo
            componentThree

        result <- SUT.runComponentM "test application" componentAction
        SUT.runTeardown_ result
        callCount <- readIORef callCountRef
        assertEqual "teardown action got called more than once" 2 callCount

    , testCase "failure runs teardown of previously allocated components" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne =
            SUT.buildComponent "one" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentTwo =
            SUT.buildComponent_ "two"
              $ throwIO (ErrorCall "failing two")


          componentThree =
            SUT.buildComponent "three" $ do
              void $ throwIO (ErrorCall "failing three")
              return ((), return () :: IO ())

          componentFour =
            SUT.buildComponent "four" $ do
              t <- SUT.newTeardown "four" (modifyIORef callCountRef (+1) :: IO ())
              return ((), t)

          componentAction = do
            componentOne
            void componentTwo
            componentThree
            componentFour

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [err]) -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
              (Just $ ErrorCall "failing two")
              (fromException err)
            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) -> do
            assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "component construction allows throwM calls" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne =
            SUT.buildComponent "one" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentTwo =
            SUT.buildComponent "two" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentAction = do
            componentOne
            void $ throwM (ErrorCall "failing via MonadThrow")
            componentTwo

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [err]) -> do
            callCount <- readIORef callCountRef
            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing via MonadThrow")
                        (fromException err)
            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) -> do
            assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "component construction allows fail calls" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne =
            SUT.buildComponent "one" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentTwo =
            SUT.buildComponent "two" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentAction = do
            componentOne
            void $ fail "failing via fail"
            componentTwo

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [err]) -> do
            callCount <- readIORef callCountRef
            assertEqual ("expected introduced error, got different one: " <> show err)
                        "failing via fail"
                        (maybe "" (\res ->
                                     case res of
                                       SUT.ComponentFailure errMsg ->
                                         errMsg
                                       _ ->
                                         "FAIL - Invalid ComponentException received")
                                  (fromException err))
            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) -> do
            assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "initializes components sequentially" $ do
        let
          componentOne =
            SUT.buildComponent "one" $ do
              tid <- myThreadId
              return (tid, return () :: IO ())

          componentTwo =
            SUT.buildComponent_ "two" $ do
              tid <- myThreadId
              return tid

          componentThree =
            SUT.buildComponent "three" $ do
              tid <- myThreadId
              return (tid, return () :: IO ())

          componentAction = do
            t1 <- componentOne
            t2 <- componentTwo
            t3 <- componentThree
            return (t1,t2,t3)


        result <- SUT.runComponentM "test application" componentAction

        let (t1, t2, t3) = SUT.fromComponent result

        assertBool "expecting t1 to be different than t2" (t1 == t2)
        assertBool "expecting t2 to be different than t3" (t2 == t3)

        tresult <- SUT.runTeardown result
        assertEqual "expecting 3 elements" 3 (toredownCount tresult)
    ]

  , testGroup "Applicative"
    [
      testCase "reports multiple failures" $ do
        callCountRef <- newIORef (0 :: Int)

        let
          componentOne =
            SUT.buildComponent "one" $ do
              return ((), modifyIORef callCountRef (+1) :: IO ())

          componentTwo =
            SUT.buildComponent_ "two" $ throwIO (ErrorCall "failing two")

          componentThree =
            SUT.buildComponent "three" $ do
              void $ throwIO (ErrorCall "failing three")
              return (0 :: Int, return () :: IO ())

          componentAction =
            componentOne
            *> componentTwo
            *> componentThree

        result <- try $ SUT.runComponentM "test application" componentAction
        case result of
          Left (SUT.ComponentStartupFailure [errTwo, errThree]) -> do
            callCount <- readIORef callCountRef

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing two")
                        (fromException errTwo)

            assertEqual "expected introduced error, got different one"
                        (Just $ ErrorCall "failing three")
                        (fromException errThree)

            assertEqual "teardown action got called more than once" 1 callCount

          Left (err :: SUT.ComponentError) -> do
            assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

          Right _ ->
            assertFailure "expected error, but did not happen"

    , testCase "initializes components concurrently" $ do
        let
          componentOne =
            SUT.buildComponent "one" $ do
              tid <- myThreadId
              return (tid, return () :: IO ())

          componentTwo =
            SUT.buildComponent_ "two" $ do
              tid <- myThreadId
              return tid

          componentThree =
            SUT.buildComponent "three" $ do
              tid <- myThreadId
              return (tid, return () :: IO ())

          componentAction =
            (,,) <$> componentOne <*> componentTwo <*> componentThree


        result <- SUT.runComponentM "test application" componentAction

        let (t1, t2, t3) = SUT.fromComponent result

        assertBool "expecting t1 to be different than t2" (t1 /= t2)
        assertBool "expecting t2 to be different than t3" (t2 /= t3)
        assertBool "expecting t1 to be different than t3" (t1 /= t3)

        tresult <- SUT.runTeardown result
        assertEqual "expecting 3 elements" 3 (toredownCount tresult)
    ]
  ]
