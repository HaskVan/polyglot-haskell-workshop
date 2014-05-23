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
  toJSON = undefined

instance FromJSON Exp where
  parseJSON = undefined
