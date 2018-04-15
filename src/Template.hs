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
      H.title "Goals"
      link ! rel "stylesheet" ! href "/bulma.css"
    body $ do
      h1 "Goals"
      p "Manage projects one goal at a time"
