module Main where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
  haskellFiles <- glob "src/**/*.hs"
  doctest haskellFiles
