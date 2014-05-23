module Types where

data Exp
  = Sum Exp Exp
  | Prod Exp Exp
  | Number Int
  deriving (Show, Eq, Ord)
