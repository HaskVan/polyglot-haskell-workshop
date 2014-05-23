{-# LANGUAGE OverloadedStrings #-}
module ParserTest where

import Parser (numberParser, sumParser, prodParser, parenParser, expParser)
import Exp (eval)
import qualified Data.Attoparsec.Text as Parser

import Test.Hspec (describe, it, Spec)
import Test.Hspec.QuickCheck (prop)
import Test.HUnit (assertEqual)

import Types
import TestTypes ()

tests :: Spec
tests = do
  describe "numberParser" $
    it "parses an int without signs" $ do
      assertEqual "should parse number"
                  (Right $ Number 31)
                  (Parser.parseOnly numberParser "31")

  describe "parensParser" $
    it "parses an expression inside parenthesis" $ do
      assertEqual "should parse operation"
                  (Right $ Sum (Number 31) (Number 4))
                  (Parser.parseOnly parenParser "(31 + 4)")

  describe "sumParser" $ do
    it "parses a sum of two numbers" $
      assertEqual "should parse sum with numbers"
                  (Right $ (Sum (Number 31) (Number 10)))
                  (Parser.parseOnly sumParser "31 + 10")

    it "parses a sum of exp and number" $
      assertEqual "should parse sum with number and exp"
                  (Right $ (Sum (Sum (Number 31) (Number 10))
                                (Number 3)))
                  (Parser.parseOnly sumParser "(31 + 10) + 3")

    it "parses a sum of number and exp" $
      assertEqual "should parse sum with number and exp"
                  (Right $ (Sum (Number 3)
                                (Sum (Number 31) (Number 10))))
                  (Parser.parseOnly sumParser "3 + (31 + 10)")

  describe "prodParser" $ do
    it "parses a prod of two numbers" $
      assertEqual "should parse sum with numbers"
                  (Right $ (Prod (Number 31) (Number 10)))
                  (Parser.parseOnly prodParser "31 * 10")

    it "parses a prod of exp and number" $
      assertEqual "should parse sum with number and exp"
                  (Right $ (Prod (Prod (Number 31) (Number 10))
                                 (Number 3)))
                  (Parser.parseOnly prodParser "(31 * 10) * 3")

    it "parses a prod of number and exp" $
      assertEqual "should parse sum with number and exp"
                  (Right $ (Prod (Number 3)
                                 (Prod (Number 31) (Number 10))))
                  (Parser.parseOnly prodParser "3 * (31 * 10)")

  describe "expParser" $ do
    it "parses complex nestings" $
      assertEqual "should parse correctly"
                  (Right $ (Sum (Sum (Number 31)
                                      (Sum (Number 2) (Number 2)))
                                (Prod (Number 10)
                                      (Prod (Number 3) (Number 5)))))
                  (Parser.parseOnly expParser "(31 + (2 + 2)) + (10 * (3 * 5))")

  prop "sums are commutative"
    $ \exp1 exp2 -> eval (Sum exp1 exp2) == eval (Sum exp2 exp1)

  prop "products are commutative"
    $ \exp1 exp2 -> eval (Prod exp1 exp2) == eval (Prod exp2 exp1)
