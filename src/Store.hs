module Store where

import Database.Persist
import Database.Persist.Sqlite
import Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

getAllProjects :: ConnectionPool -> IO [Entity Project]
getAllProjects = runSqlPool (selectList [] [])

findProject :: ConnectionPool -> Key Project -> IO (Maybe (Entity Project))
findProject pool pjId = runSqlPool (getEntity pjId) pool

createProject :: ConnectionPool -> Project -> IO (Key Project)
createProject pool pj = runSqlPool (insert pj) pool

deleteProject :: ConnectionPool -> Key Project -> IO ()
deleteProject pool pjId = runSqlPool (delete pjId) pool

updateProject :: ConnectionPool -> Key Project -> Project -> IO ()
updateProject pool projectId newProject =
  runSqlPool (replace projectId newProject) pool

findGoals :: ConnectionPool -> Key Project -> IO [Entity Goal]
findGoals pool pjId = runSqlPool (selectList [GoalProjectId ==. pjId] []) pool

createGoal ::
     ConnectionPool -> Key Project -> GoalWithoutProjectId -> IO (Key Goal)
createGoal pool pjId goalWithoutPjId = runSqlPool (insert goal) pool
  where
    goal = Goal pjId (description goalWithoutPjId)

deleteGoal :: ConnectionPool -> Key Goal -> IO ()
deleteGoal pool goalId = runSqlPool (delete goalId) pool
