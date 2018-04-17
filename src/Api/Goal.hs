{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Goal where

import Schema
import Servant

type PostGoals
   = "projects" :> Capture "project_id" (Key Project) :> "goals" :> ReqBody '[ JSON] GoalWithoutProjectId :> PostCreated '[ JSON] (Key Goal)

type DeleteGoal
   = "goals" :> Capture "goal_id" (Key Goal) :> DeleteNoContent '[ JSON] NoContent

type PutGoal
   = "goals" :> Capture "goal_id" (Key Goal) :> ReqBody '[ JSON] GoalWithoutProjectId :> PutNoContent '[ JSON] NoContent

type GoalApi = PostGoals :<|> DeleteGoal :<|> PutGoal
