{-# LANGUAGE OverloadedStrings #-}
module JSONTest where


import Data.Monoid (mconcat)

import Data.Aeson (eitherDecode, encode)
import Test.Hspec (describe, it, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.HUnit (assertEqual)

import Exp.JSON ()
import Types
import TestTypes ()

tests :: Spec
tests =
  describe "json parsing" $ do
    describe "number" $
      it "parses a Number exp" $
        assertEqual "should parse to a Number exp"
                    (Right [Number 3])
                    (eitherDecode "[3]")

    describe "string" $
      it "uses the Parser for Exp" $
        assertEqual "should use parser"
                    (Right [Sum (Number 3) (Prod (Number 1) (Number 5))])
                    (eitherDecode "[\"3 + (1 * 5)\"]")

    describe "object" $
      it "parses prod/sum/number correctly" $
        assertEqual
          "should use parser"
          (Right $  Prod (Number 3) (Sum (Number 0) (Number 5)))
          (eitherDecode $ mconcat [ "{\"type\":\"prod\", \"a\": 3,"
                                  , "\"b\": "
                                  , "{ \"type\": \"sum\", \"a\":0, \"b\": 5 }}" ])

    prop "serialization/deseralization works always" $
      \xs -> eitherDecode (encode xs) == Right (xs :: Exp)
