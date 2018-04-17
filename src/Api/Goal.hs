{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Goal where

import Schema
import Servant

type PostGoals
   = ReqBody '[ JSON] GoalWithoutProjectId :> PostCreated '[ JSON] (Key Goal)

type DeleteGoal
   = Capture "goal_id" (Key Goal) :> DeleteNoContent '[ JSON] NoContent

type GoalApi
   = "projects" :> Capture "project_id" (Key Project) :> "goals" :> PostGoals :<|> "goals" :> DeleteGoal
