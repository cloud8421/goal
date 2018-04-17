{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Goal where

import Schema
import Servant

type PostGoals
   = ReqBody '[ JSON] GoalWithoutProjectId :> PostCreated '[ JSON] (Key Goal)

type GoalApi
   = "projects" :> Capture "project_id" (Key Project) :> "goals" :> PostGoals
