module Philosophers.MVar where

import Control.Concurrent (threadDelay, MVar)
import Control.Concurrent.Async (async, waitAnyCancel, Async)
import Control.Applicative ((<$>))
import Control.Monad (forM, forever, void)
import Data.Vector ((!))
import qualified Data.Vector as V

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

import qualified Control.Concurrent.MVar as MVar

type Fork = MVar ()

data Philosopher
  = Philosopher {
    _name      :: !String
  , _leftFork  :: !Fork
  , _rightFork :: !Fork
  }

takeFork :: Fork -> IO ()
takeFork = MVar.takeMVar

releaseFork :: Fork -> IO ()
releaseFork fork = MVar.putMVar fork ()

philosopherDinner :: Philosopher -> IO (Async ())
philosopherDinner philosopher =
  async
  $ forever
  $ do
    takeFork $ _leftFork philosopher
    takeFork $ _rightFork philosopher

    putStrLn $ "Philosopher " ++ show (_name philosopher) ++ " is eating"
    threadDelay 3000111

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
  forks <- V.fromList <$> mapM (const $ MVar.newMVar ()) [0 .. pred n]
  forM [0..n] $ \i ->
    let leftFork  = forks ! (i `mod` n)
        rightFork = forks ! (succ i `mod` n)
    in return $ Philosopher (show i) leftFork rightFork
