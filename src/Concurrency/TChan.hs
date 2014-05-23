module Concurrency.TChan where

import Control.Monad (void)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan as TChan

consumer :: TChan Int -> IO ()
consumer chan = do
  msg <- atomically $ TChan.readTChan chan
  print msg
  consumer chan

producer :: TChan Int -> IO ()
producer chan = loop 0
  where
    loop msg = do
      atomically $ TChan.writeTChan chan msg
      threadDelay 1000111
      loop $ succ msg

start :: IO ()
start = do
  chan <- TChan.newTChanIO
  consumerAsync <- async $ consumer chan
  producerAsync <- async $ producer chan
  -- void $ waitBoth consumerAsync producerAsync
  void $ waitAnyCancel [consumerAsync, producerAsync]
