module Exp where

data Exp
  = Sum Exp Exp
  | Prod Exp Exp
  | Number Int
  deriving (Show, Eq, Ord)

eval :: Exp -> Int
eval = undefined
