{-# LANGUAGE OverloadedStrings #-}

module LibSpec
  ( spec
  ) where

import Lib (app)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

application :: IO Application
application = return app

spec :: Spec
spec =
  with application $
  describe "app" $ do
    let req = get "/"
    it "GET / responds successfully" $ req `shouldRespondWith` 200
    it "GET / responds with correct body" $
      req `shouldRespondWith` "Hello, World!"
