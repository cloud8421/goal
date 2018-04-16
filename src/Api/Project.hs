{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Project where

import Database.Persist.Sqlite (Entity)
import Schema
import Servant

type GetProjects = "projects" :> Get '[ JSON] [Entity Project]

type PostProjects
   = "projects" :> ReqBody '[ JSON] Project :> PostCreated '[ JSON] (Key Project)

type ProjectApi = GetProjects :<|> PostProjects
