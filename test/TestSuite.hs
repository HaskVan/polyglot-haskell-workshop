module Main where

import Foo (foo)
import qualified ParserTest
import qualified JSONTest

import Test.Hspec.Runner (hspec)
import Test.Hspec (describe, it, Spec)
import Test.HUnit (assertEqual)

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
  describe "foo example"
    $ it "must return foo"
      $ assertEqual "foo should equal foo" "foo" foo
  ParserTest.tests
  JSONTest.tests
