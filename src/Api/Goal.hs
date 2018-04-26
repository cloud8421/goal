{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Goal where

import Database.Persist.Sqlite (Entity)
import Schema
import Servant

type PostGoals
   = "projects" :> Capture "project_id" (Key Project) :> "goals" :> ReqBody '[ JSON] GoalWithoutProjectId :> PostCreated '[ JSON] (Key Goal)

type GetGoals
   = "projects" :> Capture "project_id" (Key Project) :> "goals" :> Get '[ JSON] [Entity Goal]

type GetGoal
   = "goals" :> Capture "goal_id" (Key Goal) :> Get '[ JSON] GoalWithActions

type DeleteGoal
   = "goals" :> Capture "goal_id" (Key Goal) :> DeleteNoContent '[ JSON] NoContent

type PutGoal
   = "goals" :> Capture "goal_id" (Key Goal) :> ReqBody '[ JSON] GoalWithoutProjectId :> PutNoContent '[ JSON] NoContent

type GoalApi = PostGoals :<|> GetGoals :<|> GetGoal :<|> DeleteGoal :<|> PutGoal
