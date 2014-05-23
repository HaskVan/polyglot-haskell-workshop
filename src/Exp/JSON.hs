{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exp.JSON where

import Control.Applicative ((<$>), (<*>))
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson


import Parser (parseExp)
import Types

instance ToJSON Exp where
  toJSON (Sum exp1 exp2) =
    Aeson.object [ "type" .= ("sum" :: Text)
                 , "a"    .= exp1
                 , "b"    .= exp2 ]

  toJSON (Prod exp1 exp2) =
    Aeson.object [ "type" .= ("prod" :: Text)
                 , "a"    .= exp1
                 , "b"    .= exp2 ]

  toJSON (Number n) =
    Aeson.object [ "type"  .= ("number" :: Text)
                 , "value" .=  n ]

instance FromJSON Exp where
  parseJSON value@(Aeson.Object obj) = do
    expType <- obj .: "type"
    case expType of
      Aeson.String "prod"   -> Prod   <$> obj .: "a" <*> obj .: "b"
      Aeson.String "sum"    -> Sum    <$> obj .: "a" <*> obj .: "b"
      Aeson.String "number" -> Number <$> obj .: "value"
      _                     -> typeMismatch "Exp" value

  parseJSON value@(Aeson.Number n) =
    case floatingOrInteger n of
      Right integer -> return $ Number integer
      Left _ -> typeMismatch "Exp.Number" value

  parseJSON value@(Aeson.String s) =
    case parseExp s of
      Right result -> return result
      Left  err    -> typeMismatch ("Exp (" ++ show err ++ ")") value
  parseJSON value = typeMismatch "Exp" value
