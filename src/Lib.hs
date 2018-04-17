{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app
  , migrate
  , startServer
  ) where

import Api.Goal
import Api.Project
import Config
import Control.Exception (SomeException(..))
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sqlite (ConnectionPool, withSqlitePool)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import qualified Schema
import Servant
import qualified Store as S
import qualified Template as T

type API = T.Root :<|> "api" :> (ProjectApi :<|> GoalApi)

api :: Proxy API
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api (server pool)

server :: ConnectionPool -> Server API
server pool = homePage :<|> (projectsApi :<|> goalsApi)
  where
    projectsApi =
      getAllProjects :<|> getProject :<|> createProject :<|> deleteProject
    goalsApi = createGoal
    homePage = return T.home
    getAllProjects = liftIO $ S.getAllProjects pool
    getProject projectId = do
      maybeProject <- liftIO $ S.findProject pool projectId
      goals <- liftIO $ S.findGoals pool projectId
      case maybeProject of
        Nothing -> Handler $ throwError err404
        Just project -> return $ Schema.ProjectWithGoals project goals
    createProject project = liftIO $ S.createProject pool project
    deleteProject projectId = do
      liftIO $ S.deleteProject pool projectId
      return NoContent
    createGoal projectId g =
      liftIO (S.createGoal pool projectId g) `catch`
      (\(SomeException _) -> Handler $ throwError err400)

migrate :: Config -> IO ()
migrate config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 1 $ \pool ->
    liftIO $ S.runMigrations pool

staticMiddleware :: Middleware
staticMiddleware = staticPolicy (noDots >-> addBase "static")

startServer :: Config -> IO ()
startServer config =
  runStderrLoggingT $
  withSqlitePool (configDbPath config) 5 $ \pool ->
    liftIO $
    run (configPort config) $ logStdoutDev $ staticMiddleware $ app pool
