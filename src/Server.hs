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
processJSON = Web.post "/exp.json" $ do
  expression <- Web.jsonData
  Web.json
    $ Aeson.object [ "expression" .= (expression :: Exp)
                   , "eval"       .= eval expression ]

renderForm :: ScottyM ()
renderForm = Web.get "/exp" $
  Web.raw
    $ renderHtml
    $ H.docTypeHtml $ do
      H.head $
       H.title "Exp Evaluator"

      H.body $
        H.form ! A.method "POST"
               ! A.action "/exp" $ do

          H.label ! A.for "exp" $ "Introduce Expression"

          H.input ! A.type_ "text"
                  ! A.id "exp"
                  ! A.name  "exp"

          H.input ! A.type_ "submit"

processForm :: ScottyM ()
processForm = Web.post "/exp" $ do
  expression <- Web.param "exp"
  Web.setHeader "Content-Type" "text/plain"
  Web.raw
    $ Aeson.encode
    $ Aeson.object [ "expression" .= (expression :: Exp)
                   , "eval"       .= eval expression ]

server :: ScottyM ()
server = do
  renderForm
  processForm
  processJSON
