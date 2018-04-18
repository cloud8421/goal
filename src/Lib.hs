module Lib
  ( app
  , migrate
  , startServer
  ) where

import Api
import Config
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (ConnectionPool, withSqlitePool)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import Servant
import Store

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api (server pool)

migrate :: Config -> IO ()
migrate config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 1 $ \pool -> liftIO $ runMigrations pool

staticMiddleware :: Middleware
staticMiddleware = staticPolicy (noDots >-> addBase "static")

startServer :: Config -> IO ()
startServer config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 5 $ \pool ->
    liftIO $
    run (configPort config) $ logStdoutDev $ staticMiddleware $ app pool
