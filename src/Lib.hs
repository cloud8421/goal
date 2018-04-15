{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app
  , migrate
  , startServer
  ) where

import Api.Project
import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Store

type API = "api" :> ProjectApi

api :: Proxy API
api = Proxy

app :: Text -> Application
app dbPath = serve api server

server :: Server API
server = return []

migrate :: Config -> IO ()
migrate config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 1 $ \pool -> liftIO $ runMigrations pool

startServer :: Config -> IO ()
startServer config = run (configPort config) (app (configDbPath config))
