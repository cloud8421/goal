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
import Database.Persist.Sqlite (ConnectionPool, withSqlitePool)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Store as S

type API = "api" :> ProjectApi

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api (server pool)

server :: ConnectionPool -> Server API
server pool = getAllProjects :<|> createProject
  where
    getAllProjects = liftIO $ S.getAllProjects pool
    createProject project = liftIO $ S.createProject pool project

migrate :: Config -> IO ()
migrate config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 1 $ \pool ->
    liftIO $ S.runMigrations pool

startServer :: Config -> IO ()
startServer config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 5 $ \pool ->
    liftIO $ run (configPort config) $ logStdoutDev $ app pool
