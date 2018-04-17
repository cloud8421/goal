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
import Database.Persist (Entity(..))
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )
import GHC.Generics

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Project json
    name Text
    deriving Eq Show Generic
Goal json
    projectId ProjectId
    description Text
    deriving Eq Show Generic
|]

data ProjectWithGoals = ProjectWithGoals
  { project :: Entity Project
  , goals :: [Entity Goal]
  }

data GoalWithoutProjectId = GoalWithoutProjectId
  { description :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GoalWithoutProjectId

instance FromJSON GoalWithoutProjectId

instance ToJSON ProjectWithGoals where
  toJSON projectWithGoals =
    object
      ["id" .= uid, "name" .= projectName pj, "goals" .= goals projectWithGoals]
    where
      (Entity uid pj) = project projectWithGoals
