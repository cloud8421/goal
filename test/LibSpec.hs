{-# LANGUAGE OverloadedStrings #-}

module LibSpec
  ( spec
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text, unpack)
import Database.Persist.Sqlite
  ( SqliteConnectionInfo
  , mkSqliteConnectionInfo
  , walEnabled
  , withSqlitePoolInfo
  )
import Lens.Micro (set)
import Lib (app)
import Network.Wai (Application)
import Store (runMigrations)
import System.Directory (removeFile)
import Test.Hspec
import Test.Hspec.Wai

dbPath :: Text
dbPath = "data/test.db"

connInfo :: SqliteConnectionInfo
connInfo = set walEnabled False (mkSqliteConnectionInfo dbPath)

application :: IO Application
application =
  runNoLoggingT $ withSqlitePoolInfo connInfo 1 $ \pool -> return $ app pool

migrateDb :: IO ()
migrateDb =
  runNoLoggingT $
  withSqlitePoolInfo connInfo 1 $ \pool -> liftIO $ runMigrations pool

removeDbFile :: IO ()
removeDbFile = removeFile $ unpack dbPath

spec :: Spec
spec =
  beforeAll_ migrateDb $
  afterAll_ removeDbFile $
  with application $
  describe "GET /api/projects" $ do
    let req = get "/api/projects"
    it "it responds successfully" $ req `shouldRespondWith` 200
    it "it responds with []" $ req `shouldRespondWith` "[]"
