{-# LANGUAGE OverloadedStrings #-}
module Server where

import Web.Scotty (ScottyM)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Web.Scotty as Web
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Types (Exp(..))
import Exp (eval)
import Exp.JSON ()
import Exp.Param ()

processJSON :: ScottyM ()
processJSON = undefined

renderForm :: ScottyM ()
renderForm = undefined

processForm :: ScottyM ()
processForm = undefined

server :: ScottyM ()
server = do
  renderForm
  processForm
  processJSON
