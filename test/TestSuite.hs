module Main where

import Foo (foo)

import Test.Hspec (hspec, describe, it)
import Test.HUnit (assertEqual)

main :: IO ()
main = hspec $ do
  describe "foo example" $
    it "must return foo" $
     assertEqual "foo should equal foo" "bar" foo
