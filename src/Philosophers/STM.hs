module Philosophers.STM where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, waitAnyCancel, Async)
import Control.Applicative ((<$>))
import Control.Monad (forM, forever, void)
import Data.Vector ((!))
import qualified Data.Vector as V

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import Control.Concurrent.STM (atomically, retry, TVar, STM)
import qualified Control.Concurrent.STM.TVar as TVar

type Fork = TVar (Maybe ())

data Philosopher
  = Philosopher {
    _name      :: !String
  , _leftFork  :: !Fork
  , _rightFork :: !Fork
  }

takeFork :: Fork -> STM ()
takeFork fork = do
  value <- TVar.readTVar fork
  case value of
    Nothing -> retry
    Just _  -> TVar.writeTVar fork Nothing

releaseFork :: Fork -> STM ()
releaseFork fork = do
  value <- TVar.readTVar fork
  case value of
    Nothing -> TVar.writeTVar fork (Just ())
    Just _  -> return ()

philosopherDinner :: Philosopher -> IO (Async ())
philosopherDinner philosopher =
  async
  $ forever
  $ do
    atomically $ do
      takeFork $ _leftFork philosopher
      takeFork $ _rightFork philosopher

    putStrLn $ "Philosopher " ++ show (_name philosopher) ++ " is eating"
    threadDelay 3000111

    atomically $ do
      releaseFork $ _leftFork philosopher
      releaseFork $ _rightFork philosopher

    putStrLn
      $ "Philosopher "
      ++ show (_name philosopher)
      ++ " stopped eating and started thinking"

startDinner :: Int -> IO ()
startDinner n = do
  hSetBuffering stdout LineBuffering
  ps <- philosophers n
  asyncs <- forM ps philosopherDinner
  void $ waitAnyCancel asyncs

philosophers :: Int -> IO [Philosopher]
philosophers n = do
  forks <- V.fromList <$> mapM (const $ TVar.newTVarIO $ Just ()) [0 .. pred n]
  forM [0..n] $ \i ->
    let leftFork  = forks ! (i `mod` n)
        rightFork = forks ! (succ i `mod` n)
    in return $ Philosopher (show i) leftFork rightFork
