{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LibSpec
  ( spec
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text, unpack)
import Database.Persist.Sqlite
  ( SqliteConnectionInfo
  , mkSqliteConnectionInfo
  , walEnabled
  , withSqlitePoolInfo
  )
import Lens.Micro (set)
import Lib (app)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
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
  describe "endpoints" $ do
    context "GET /" $ do
      let req = get "/"
      it "responds successfully" $ req `shouldRespondWith` 200
    context "GET /api/projects" $ do
      let req = jsonGet "/api/projects"
      let respBody = [json|[{"id":1,"name":"example"}]|]
      it "responds successfully" $ req `shouldRespondWith` 200
      it "responds with []" $ req `shouldRespondWith` respBody
    context "POST /api/projects" $ do
      let reqBody = [json|{"name":"New Project"}|]
      let req = jsonPost "/api/projects" reqBody
      it "responds successfully" $ req `shouldRespondWith` 201
    context "DELETE /api/projects/<project-id>" $ do
      let req = jsonDelete "/api/projects/1"
      it "responds successfully" $ req `shouldRespondWith` 204

jsonPost :: ByteString -> LB.ByteString -> WaiSession SResponse
jsonPost path = request methodPost path [(hContentType, "application/json")]

jsonGet :: ByteString -> WaiSession SResponse
jsonGet path = request methodGet path [(hContentType, "application/json")] ""

jsonDelete :: ByteString -> WaiSession SResponse
jsonDelete path = request methodDelete path [(hContentType, "application/json")] ""
