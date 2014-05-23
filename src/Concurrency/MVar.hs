module Concurrency.MVar where

import Control.Monad (void)

import Control.Concurrent.Async (async, waitBoth)
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

thisThread :: MVar String -> IO ()
thisThread var = do
  msg <- MVar.takeMVar var
  putStrLn msg
  MVar.putMVar var "bye"

thatThread :: MVar String -> IO ()
thatThread var = do
  MVar.putMVar var "hello"
  msg <- MVar.takeMVar var
  putStrLn msg

start :: IO ()
start = do
  var <- MVar.newEmptyMVar
  thisAsync <- async $ thisThread var
  thatAsync <- async $ thatThread var
  void $ waitBoth thisAsync thatAsync
