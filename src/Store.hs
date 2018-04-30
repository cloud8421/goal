module Store where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist
import Database.Persist.Sqlite
import Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

runMigrationsSilent :: ConnectionPool -> IO [Text]
runMigrationsSilent = runSqlPool (runMigrationSilent migrateAll)

getAllProjects :: ConnectionPool -> IO [Entity Project]
getAllProjects = runSqlPool (selectList [] [])

findProject :: ConnectionPool -> Key Project -> IO (Maybe (Entity Project))
findProject pool pjId = runSqlPool (getEntity pjId) pool

createProject :: ConnectionPool -> Project -> UTCTime -> IO (Key Project)
createProject pool pj currentTime = runSqlPool (insert pjWithTime) pool
  where
    pjWithTime = Project (projectName pj) (Just currentTime) (Just currentTime)

deleteProject :: ConnectionPool -> Key Project -> IO ()
deleteProject pool pjId = runSqlPool (deleteCascade pjId) pool

updateProject :: ConnectionPool -> Key Project -> Project -> IO ()
updateProject pool projectId newProject =
  runSqlPool (replace projectId newProject) pool

findGoals :: ConnectionPool -> Key Project -> IO [Entity Goal]
findGoals pool pjId = runSqlPool (selectList [GoalProjectId ==. pjId] []) pool

findGoal :: ConnectionPool -> Key Goal -> IO (Maybe (Entity Goal))
findGoal pool goalId = runSqlPool (getEntity goalId) pool

createGoal ::
     ConnectionPool -> Key Project -> GoalWithoutProjectId -> IO (Key Goal)
createGoal pool pjId goalWithoutPjId = runSqlPool (insert g) pool
  where
    g = Goal pjId (description goalWithoutPjId) Nothing Nothing

deleteGoal :: ConnectionPool -> Key Goal -> IO ()
deleteGoal pool goalId = runSqlPool (deleteCascade goalId) pool

updateGoal :: ConnectionPool -> Key Goal -> GoalWithoutProjectId -> IO ()
updateGoal pool goalId goalWithoutPjId =
  runSqlPool
    (update goalId [GoalDescription =. description goalWithoutPjId])
    pool

findActions :: ConnectionPool -> Key Goal -> IO [Entity Action]
findActions pool goalId =
  runSqlPool (selectList [ActionGoalId ==. goalId] []) pool

createAction ::
     ConnectionPool -> Key Goal -> ActionWithoutGoalId -> IO (Key Action)
createAction pool goalId actionWithoutGoalId = runSqlPool (insert a) pool
  where
    a = Action goalId (summary actionWithoutGoalId) Nothing Nothing

deleteAction :: ConnectionPool -> Key Action -> IO ()
deleteAction pool actionId = runSqlPool (delete actionId) pool

updateAction :: ConnectionPool -> Key Action -> ActionWithoutGoalId -> IO ()
updateAction pool actionId actionWithoutGoalId =
  runSqlPool
    (update actionId [ActionSummary =. summary actionWithoutGoalId])
    pool
