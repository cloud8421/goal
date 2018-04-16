{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Project where

import Database.Persist.Sqlite (Entity)
import Schema
import Servant

type GetProjects = Get '[ JSON] [Entity Project]

type PostProjects
   = ReqBody '[ JSON] Project :> PostCreated '[ JSON] (Key Project)

type DeleteProject
   = Capture "patient_id" (Key Project) :> DeleteNoContent '[ JSON] NoContent

type ProjectApi = "projects" :> (GetProjects :<|> PostProjects :<|> DeleteProject)
