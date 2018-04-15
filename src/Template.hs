{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Template where

import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H

type Home = H.Html

type Root = Get '[ HTML] Home

home :: Home
home =
  H.docTypeHtml $ do
    H.head $ H.title "Live to serve"
    H.body $ do
      H.h1 "Templates!"
      H.p "This will be type-checked, rendered and served"
