{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Project where

import Database.Persist.Sqlite (Entity)
import Schema
import Servant

type GetProjects = Get '[ JSON] [Entity Project]

type GetProject
   = Capture "project_id" (Key Project) :> Get '[ JSON] ProjectWithGoals

type PostProjects
   = ReqBody '[ JSON] Project :> PostCreated '[ JSON] (Key Project)

type PutProject
   = Capture "project_id" (Key Project) :> ReqBody '[ JSON] Project :> PutNoContent '[ JSON] NoContent

type DeleteProject
   = Capture "project_id" (Key Project) :> DeleteNoContent '[ JSON] NoContent

type ProjectApi
   = "projects" :> (GetProjects :<|> GetProject :<|> PutProject :<|> PostProjects :<|> DeleteProject)
