{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Project where

import Database.Persist.Sqlite (Entity)
import Schema
import Servant

type GetPatients = "projects" :> Get '[ JSON] [Entity Project]

type PostPatients
   = "projects" :> ReqBody '[ JSON] Project :> PostCreated '[ JSON] (Key Project)

type ProjectApi = GetPatients :<|> PostPatients
