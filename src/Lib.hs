{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app
  ) where

import Api.Project
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqlitePool)
import Network.HTTP.Types
import Network.Wai
import Servant
import Store

type API = "api" :> ProjectApi

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = return []

migrate :: Text -> IO ()
migrate dbPath =
  runStderrLoggingT $
  withSqlitePool dbPath 1 $ \pool -> liftIO $ runMigrations pool
