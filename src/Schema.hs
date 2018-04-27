{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Schema where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist (Entity(..))
import Database.Persist.TH
  ( mkDeleteCascade
  , mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import GHC.Generics

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Project json
    name Text
    created UTCTime Maybe default=CURRENT_TIME
    updated UTCTime Maybe default=CURRENT_TIME
    deriving Eq Show Generic
Goal json
    projectId ProjectId
    description Text
    created UTCTime Maybe default=CURRENT_TIME
    updated UTCTime Maybe default=CURRENT_TIME
    deriving Eq Show Generic
Action json
    goalId GoalId
    summary Text
    created UTCTime Maybe default=CURRENT_TIME
    updated UTCTime Maybe default=CURRENT_TIME
    deriving Eq Show Generic
|]

data ProjectWithGoals = ProjectWithGoals
  { project :: Entity Project
  , goals :: [Entity Goal]
  }

data GoalWithActions = GoalWithActions
  { goal :: Entity Goal
  , actions :: [Entity Action]
  }

data GoalWithoutProjectId = GoalWithoutProjectId
  { description :: Text
  } deriving (Eq, Show, Generic)

data ActionWithoutGoalId = ActionWithoutGoalId
  { summary :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GoalWithoutProjectId

instance FromJSON GoalWithoutProjectId

instance ToJSON ActionWithoutGoalId

instance FromJSON ActionWithoutGoalId

instance ToJSON ProjectWithGoals where
  toJSON projectWithGoals =
    object
      [ "id" .= uid
      , "name" .= projectName pj
      , "goals" .= goals projectWithGoals
      , "created" .= projectCreated pj
      , "updated" .= projectUpdated pj
      ]
    where
      (Entity uid pj) = project projectWithGoals

instance ToJSON GoalWithActions where
  toJSON goalWithActions =
    object
      [ "id" .= uid
      , "projectId" .= goalProjectId g
      , "description" .= goalDescription g
      , "actions" .= actions goalWithActions
      , "created" .= goalCreated g
      , "updated" .= goalUpdated g
      ]
    where
      (Entity uid g) = goal goalWithActions
