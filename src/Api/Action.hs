{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Action where

import Schema
import Servant

type PostActions
   = "goals" :> Capture "goal_id" (Key Goal) :> "actions" :> ReqBody '[ JSON] ActionWithoutGoalId :> PostCreated '[ JSON] (Key Action)

type DeleteAction
   = "actions" :> Capture "action_id" (Key Action) :> DeleteNoContent '[ JSON] NoContent

type PutAction
   = "actions" :> Capture "action_id" (Key Action) :> ReqBody '[ JSON] ActionWithoutGoalId :> PutNoContent '[ JSON] NoContent

type ActionApi = PostActions :<|> DeleteAction :<|> PutAction
