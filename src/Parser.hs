module Parser where

import Exp (Exp(..))
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Parser

data Exp
  = Sum Exp Exp
  | Prod Exp Exp
  | Number Int
  deriving (Show, Eq, Ord)


numberParser :: Parser.Parser Exp
numberParser = undefined
               
sumParser :: Parser.Parser Exp
sumParser = undefined
            
prodParser :: Parser.Parser Exp
prodParser = undefined

expParser :: Parser.Parser Exp
expParser = undefined

parseExp :: Text -> Either String Exp
parseExp = Parser.parseOnly expParser
