{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( server
  , API
  ) where

import Api.Action
import Api.Goal
import Api.Project
import Control.Exception (SomeException(..))
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite (ConnectionPool)
import qualified Schema
import Servant
import qualified Store as S
import qualified Template as T

type API = T.Root :<|> "api" :> (ProjectApi :<|> GoalApi :<|> ActionApi)

server :: ConnectionPool -> Server API
server pool = homePage :<|> (projectsApi :<|> goalsApi :<|> actionApi)
  where
    projectsApi =
      getAllProjects :<|> getProject :<|> putProject :<|> createProject :<|>
      deleteProject
    goalsApi = createGoal :<|> getGoal :<|> deleteGoal :<|> putGoal
    actionApi = createAction :<|> deleteAction :<|> putAction
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
    putProject projectId newProject = do
      liftIO $ S.updateProject pool projectId newProject
      return NoContent
    createGoal projectId g =
      liftIO (S.createGoal pool projectId g) `catch`
      (\(SomeException _) -> Handler $ throwError err400)
    getGoal goalId = do
      maybeGoal <- liftIO $ S.findGoal pool goalId
      actions <- liftIO $ S.findActions pool goalId
      case maybeGoal of
        Nothing -> Handler $ throwError err404
        Just goal -> return $ Schema.GoalWithActions goal actions
    deleteGoal goalId = do
      liftIO $ S.deleteGoal pool goalId
      return NoContent
    putGoal goalId newGoal = do
      liftIO $ S.updateGoal pool goalId newGoal
      return NoContent
    createAction goalId a =
      liftIO (S.createAction pool goalId a) `catch`
      (\(SomeException _) -> Handler $ throwError err400)
    deleteAction actionId = do
      liftIO $ S.deleteAction pool actionId
      return NoContent
    putAction actionId newAction = do
      liftIO $ S.updateAction pool actionId newAction
      return NoContent
