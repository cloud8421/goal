{-# LANGUAGE OverloadedStrings #-}

module LibSpec
  ( spec
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqlitePool)
import Lib (app)
import Network.Wai (Application)
import Store (runMigrations)
import Test.Hspec
import Test.Hspec.Wai

dbPath :: Text
dbPath = "data/test.db"

application :: IO Application
application =
  runNoLoggingT $ withSqlitePool dbPath 1 $ \pool -> return $ app pool

migrateDb :: IO ()
migrateDb =
  runNoLoggingT $ withSqlitePool dbPath 1 $ \pool -> liftIO $ runMigrations pool

spec :: Spec
spec =
  beforeAll_ migrateDb $
  with application $
  describe "GET /api/projects" $ do
    let req = get "/api/projects"
    it "it responds successfully" $ req `shouldRespondWith` 200
    it "it responds with []" $ req `shouldRespondWith` "[]"
