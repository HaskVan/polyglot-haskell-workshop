module Monad where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type DB = HashMap UserId User
type UserId = Int

data User
  = User {
    _userId         :: UserId
  , _userCouple     :: UserId
  , _userBestFriend :: UserId
  }

globalDB :: DB
globalDB =
  HashMap.fromList [
      (1, User 1 2 4)
    , (2, User 2 1 3)
    , (3, User 3 5 2)
    , (4, User 4 8 1)
    ]

couplesBestFriend :: UserId -> DB -> Maybe UserId
couplesBestFriend uid db =
  case HashMap.lookup uid db of
    Nothing -> Nothing
    Just me ->
      case HashMap.lookup (_userCouple me) db of
        Nothing -> Nothing
        Just couple ->
          case HashMap.lookup (_userBestFriend couple) db of
            Nothing -> Nothing
            Just bestFriend -> Just $ _userId bestFriend

couplesBestFriendFM :: UserId -> DB -> Maybe UserId
couplesBestFriendFM uid db =
  HashMap.lookup uid db `flatMap` \me ->
    HashMap.lookup (_userCouple me) db `flatMap` \couple ->
      HashMap.lookup (_userBestFriend couple) db `flatMap` \bff ->
        Just $ _userId bff

flatMap :: Maybe a -> (a -> Maybe b) -> Maybe b
flatMap = undefined

{-
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> ma
-}

couplesBestFriendDo :: UserId -> DB -> Maybe UserId
couplesBestFriendDo uid db = do
  me     <- HashMap.lookup uid db
  couple <- HashMap.lookup (_userCouple me) db
  bff    <- HashMap.lookup (_userBestFriend couple) db
  return $ _userId bff



flatMapL :: [a] -> (a -> [b]) -> [b]
flatMapL = undefined

allCombinationsFM :: Num a => [a] -> [a] -> [a]
allCombinationsFM xs ys =
  xs `flatMapL` \x ->
    ys `flatMapL` \y ->
      [x + y]

allCombinationsDo :: Num a => [a] -> [a] -> [a]
allCombinationsDo xs ys =  do
  x <- xs
  y <- ys
  return $ x + y

allCombinationsLC :: Num a => [a] -> [a] -> [a]
allCombinationsLC xs ys = [ x + y | x <- xs, y <- ys ]
