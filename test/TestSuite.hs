module Main where

import Foo (foo)

import Control.Monad (void)

import Test.Hspec.Runner (hspecResult)
import Test.Hspec (describe, it, Spec)
import Test.HUnit (assertEqual)

main :: IO ()
main = void $ hspecResult specs

specs :: Spec
specs = do
  describe "foo example" $
    it "must return foo" $
     assertEqual "foo should equal foo" "foo" foo
