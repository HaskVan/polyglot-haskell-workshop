module Parser where

import Control.Applicative ((*>), (<*), (<*>), (<$>))
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Parser

import Types

numberParser :: Parser.Parser Exp
numberParser = do
  Parser.skipSpace
  number <- Parser.decimal
  return $ Number number

parenParser :: Parser.Parser Exp
parenParser =
  Parser.skipSpace *>
  Parser.char '('  *>
  expParser        <*
  Parser.char ')'

sumParser :: Parser.Parser Exp
sumParser =
  Sum <$> Parser.choice [parenParser, numberParser]
      <*> (Parser.skipSpace
           *> Parser.char '+'
           *> Parser.skipSpace
           *> Parser.choice [parenParser, numberParser])

prodParser :: Parser.Parser Exp
prodParser =
  Prod <$> Parser.choice [parenParser, numberParser]
       <*> (Parser.skipSpace
            *> Parser.char '*'
            *> Parser.skipSpace
            *> Parser.choice [parenParser, numberParser])


expParser :: Parser.Parser Exp
expParser = Parser.choice [ sumParser
                          , prodParser
                          , parenParser
                          , numberParser]

parseExp :: Text -> Either String Exp
parseExp = Parser.parseOnly expParser
