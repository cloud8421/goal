module Config where

import Data.Text (Text)

data Config = Config
  { configDbPath :: Text
  , configPort :: Int
  , configEmbedded :: Bool
  }
