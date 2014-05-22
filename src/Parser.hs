module Parser where

import Exp (Exp(..))
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Parser

numberParser :: Parser.Parser Exp
numberParser = do
  Parser.skipSpace
  number <- Parser.decimal
  return $ Number number

parenParser :: Parser.Parser Exp
parenParser = do
  Parser.skipSpace
  Parser.char '('
  inner <- expParser
  Parser.char ')'
  return inner

sumParser :: Parser.Parser Exp
sumParser = do
  exp1 <- Parser.choice [parenParser, numberParser]
  Parser.skipSpace
  Parser.char '+'
  Parser.skipSpace
  exp2 <- Parser.choice [parenParser, numberParser]
  return $ Sum exp1 exp2

prodParser :: Parser.Parser Exp
prodParser = do
  exp1 <- Parser.choice [parenParser, numberParser]
  Parser.skipSpace
  Parser.char '*'
  Parser.skipSpace
  exp2 <- Parser.choice [parenParser, numberParser]
  return $ Prod exp1 exp2


expParser :: Parser.Parser Exp
expParser = Parser.choice [ sumParser
                          , prodParser
                          , parenParser
                          , numberParser]

parseExp :: Text -> Either String Exp
parseExp = Parser.parseOnly expParser
