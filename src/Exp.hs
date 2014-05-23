module Exp where

import Types

eval :: Exp -> Int
eval (Number n) = n
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
