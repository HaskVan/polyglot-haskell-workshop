module ParserTest where

import Test.Hspec (describe, it, pending, Spec)
-- import Test.HUnit (assertEqual)

tests :: Spec
tests = do
  describe "parseNumber" $
    it "parses an int without signs" pending

  describe "parseSum" $ do
    it "parses a sum of two numbers" pending
    it "parses a sum of exp and number" pending
    it "parses a sum of number and exp" pending

  describe "parseProd" $ do
    it "parses a prod of two numbers" pending
    it "parses a prod of exp and number" pending
    it "parses a prod of number and exp" pending
