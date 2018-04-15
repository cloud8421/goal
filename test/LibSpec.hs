{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Schema (Project(..))
import Store (createProject, runMigrations)
import System.Directory (removeFile)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

dbPath :: Text
dbPath = "data/test.db"

connInfo :: SqliteConnectionInfo
connInfo = set walEnabled False (mkSqliteConnectionInfo dbPath)

application :: IO Application
application =
  runNoLoggingT $ withSqlitePoolInfo connInfo 1 $ \pool -> return $ app pool

seedDb :: IO ()
seedDb =
  runNoLoggingT $
  withSqlitePoolInfo connInfo 1 $ \pool -> do
    liftIO $ runMigrations pool
    _ <- liftIO $ createProject pool (Project "example")
    return ()

removeDbFile :: IO ()
removeDbFile = removeFile $ unpack dbPath

spec :: Spec
spec =
  beforeAll_ seedDb $
  afterAll_ removeDbFile $
  with application $
  describe "GET /api/projects" $ do
    let req = get "/api/projects"
    let respBody = [json|[{"id":1,"name":"example"}]|]
    it "it responds successfully" $ req `shouldRespondWith` 200
    it "it responds with []" $ req `shouldRespondWith` respBody
