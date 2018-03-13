{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Test.Tasty                   (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
import Test.Tasty.Runners           (consoleTestReporter, listingTests)

import Control.Concurrent (myThreadId)
import Control.Exception (ErrorCall(..))
import Control.Teardown (newTeardown)

import Test.Tasty.HUnit (assertBool, assertFailure, assertEqual, testCase)
import Control.Teardown (toredownCount)

import qualified Control.Monad.Component as SUT

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "componentm library"
      [ tests ]
    )

tests :: TestTree
tests =
  testGroup "ComponentM"
  [
    testCase "component with same name returns cached result on multiple calls" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          SUT.buildComponent "one" $ do
            modifyIORef callCountRef (+1) :: IO ()
            return ((), error "i'm going to fail" :: IO ())

      SUT.withComponentM2
        "test application" (replicateM_ 30 componentOne)
        (const $ return ())
        (\_teardownResult -> do
            callCount <- readIORef callCountRef
            assertBool "teardown action got called more than once" (callCount <= 1))

  , testCase "aggregates multiple component teardown values" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          SUT.buildComponent "one" $ do
            return ((), modifyIORef callCountRef (+1) :: IO ())

        componentTwo =
          SUT.buildComponent "two" $ do
            t <- newTeardown "internal" (modifyIORef callCountRef (+1) :: IO ())
            return ((), t)

        componentThree =
          SUT.buildComponent_ "three" $ return ()

        componentBuilder = do
          componentOne
          componentTwo
          componentThree

      SUT.withComponentM2
        "test application" componentBuilder
        (const $ return ())
        (\_teardownResult -> do
            callCount <- readIORef callCountRef
            assertEqual "teardown action got called more than once" 2 callCount)


  , testCase "failure runs teardown of previously allocated components" $ do
      callCountRef <- newIORef (0 :: Int)

      let
        componentOne =
          SUT.buildComponent "one" $ do
            return ((), modifyIORef callCountRef (+1) :: IO ())

        componentTwo =
          SUT.buildComponent_ "two" $ do
            void $ throwIO (ErrorCall "failing two")
            return ()


        componentThree =
          SUT.buildComponent "three" $ do
            void $ throwIO (ErrorCall "failing three")
            return ((), return () :: IO ())

        componentFour =
          SUT.buildComponent "four" $ do
            t <- newTeardown "four" (modifyIORef callCountRef (+1) :: IO ())
            return ((), t)

        componentBuilder = do
          componentOne
          void componentTwo
          componentThree
          componentFour

      result <- try $ SUT.withComponentM "test application" componentBuilder (const $ return ())
      case result of
        Left (SUT.ComponentInitFailure err _buildInfo) -> do
          callCount <- readIORef callCountRef
          assertEqual "expected introduced error, got different one"
            (Just $ ErrorCall "failing two")
            (fromException err)
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: SUT.ComponentError) -> do
          assertFailure $ "expected ComponentInitFailure exception, got instead: " <> show err

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

        componentBuilder = do
          componentOne
          void $ throwM (ErrorCall "failing via MonadThrow")
          componentTwo

      result <- try $ SUT.withComponentM "test application" componentBuilder (const $ return ())
      case result of
        Left (SUT.ComponentBuildFailure err) -> do
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

        componentBuilder = do
          componentOne
          void $ fail "failing via fail"
          componentTwo

      result <- try $ SUT.withComponentM "test application" componentBuilder (const $ return ())
      case result of
        Left (SUT.ComponentBuildFailure _err) -> do
          callCount <- readIORef callCountRef
          assertEqual "teardown action got called more than once" 1 callCount

        Left (err :: SUT.ComponentError) -> do
          assertFailure $ "expected ComponentStartupFailure exception, got instead: " <> show err

        Right _ ->
          assertFailure "expected error, but did not happen"

  , testCase "initializes components sequentially (not threading)" $ do
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

        componentBuilder = do
          t1 <- componentOne
          t2 <- componentTwo
          t3 <- componentThree
          return (t1,t2,t3)

      SUT.withComponentM2 "test application" componentBuilder
        (\((t1,t2,t3), _buildInto) -> do
            assertBool "expecting t1 to be different than t2" (t1 == t2)
            assertBool "expecting t2 to be different than t3" (t2 == t3))
        (\tresult ->
           assertEqual "expecting 3 elements" 3 (toredownCount tresult))

  ]
