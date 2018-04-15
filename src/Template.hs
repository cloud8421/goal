{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Template where

import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

type Home = H.Html

type Root = Get '[ HTML] Home

home :: Home
home =
  docTypeHtml $ do
    H.head $ do
      meta ! charset "UTF-8"
      H.title "Goals"
      link ! rel "stylesheet" ! href "/bulma.css"
    body $ main ! A.id "app" $ h1 "Loading..."
    script ! type_ "text/javascript" ! src "/main.js" $ mempty
    script ! type_ "text/javascript" ! src "/boot.js" $ mempty
