{-# LANGUAGE OverloadedStrings #-}

module Lib (app) where

import           Network.HTTP.Types
import           Network.Wai

app :: Application
app _ respond =
  respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
