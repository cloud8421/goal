{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app
  ) where

import Api.Project
import Network.HTTP.Types
import Network.Wai
import Servant

type API = "api" :> ProjectApi

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = return []
