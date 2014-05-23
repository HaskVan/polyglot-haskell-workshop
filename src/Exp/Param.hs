{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exp.Param where

import Data.Monoid (mconcat)
import Data.Text.Lazy (toChunks, pack)
import Web.Scotty (Parsable(..))

import Types (Exp(..))
import Parser (parseExp)

instance Parsable Exp where
  parseParam input =
    case parseExp (mconcat $ toChunks input) of
      Right result -> Right result
      Left err     -> Left $ pack err
