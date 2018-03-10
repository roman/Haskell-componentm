{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Foobar where

import RIO
import qualified Prelude
import Control.Teardown (runTeardown, renderTeardownReport)
import Control.Concurrent (forkIO, killThread)
import Control.Monad.Component (ComponentM, buildComponent, buildComponent_, runComponentM, getTeardown, fromComponent)


compA :: Show a => ComponentM (a -> IO ())
compA = buildComponent "A" $ do
  inputQueue <- newTBQueueIO 10
  tid <- forkIO $ forever (atomically (readTBQueue inputQueue) >>= Prelude.print)
  return (atomically . writeTBQueue inputQueue, killThread tid)


compB :: ComponentM (Int -> IO ())
compB = buildComponent "B" $ do
  inputQueue <- newTBQueueIO 10
  tid <- forkIO $ loop inputQueue 0
  return (atomically . writeTBQueue inputQueue, killThread tid)
  where
    loop inputQueue acc = do
      n <- atomically (readTBQueue inputQueue)
      Prelude.print (acc + n) >> loop inputQueue (acc + n)

compC :: ComponentM Text
compC = buildComponent_ "C" $ do
  return "hello world"

compD :: ComponentM Text
compD = buildComponent_ "D" $ do
  return "Hola Mundo"

buildThreads :: Show a => ComponentM (a -> IO (), Int -> IO ())
buildThreads =
  (,) <$> compA <*> compB

buildApp :: Show a => ComponentM (a -> IO (), Int -> IO ())
buildApp = do
  _useless1 <- compC
  _useless2 <- compD
  buildThreads

main :: IO ()
main = do
  component <- runComponentM "my app" buildApp
  let (writeA, writeB) = fromComponent component
  writeA ("Hello From Here" :: Text)
  writeB 1
  writeB 2
  writeB 3
  threadDelay 500
  result <- runTeardown (getTeardown component)
  Prelude.print (renderTeardownReport result)
