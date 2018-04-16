module Store where

import Database.Persist
import Database.Persist.Sqlite
import Schema

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

getAllProjects :: ConnectionPool -> IO [Entity Project]
getAllProjects = runSqlPool (selectList [] [])

createProject :: ConnectionPool -> Project -> IO (Key Project)
createProject pool project = runSqlPool (insert project) pool

deleteProject :: ConnectionPool -> Key Project -> IO ()
deleteProject pool projectId = runSqlPool (delete projectId) pool
