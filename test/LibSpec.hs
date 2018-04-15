{-# LANGUAGE OverloadedStrings #-}

module LibSpec
  ( spec
  ) where

import Lib (app)
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

application :: IO Application
application = return $ app ":memory:"

spec :: Spec
spec =
  with application $
  describe "GET /api/projects" $ do
    let req = get "/api/projects"
    it "it responds successfully" $ req `shouldRespondWith` 200
    it "it responds with []" $ req `shouldRespondWith` "[]"
