{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Project where

import Data.Aeson
import Data.Aeson.TH
import Servant

newtype Project = Project
  { projectId :: Int
  }

$(deriveJSON defaultOptions ''Project)

type ProjectApi = "projects" :> Get '[ JSON] [Project]
