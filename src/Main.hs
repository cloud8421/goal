{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad (join)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Lib
import Options.Applicative

defaultPort :: Int
defaultPort = 8080

defaultDbPath :: Text
defaultDbPath = "goals.db"

parser :: Parser (IO ())
parser =
  hsubparser
    (command "start" (info (startServer <$> configParser) startDesc) <>
     command "migrate" (info (migrate <$> configParser) migrateDesc))
  where
    configParser = Config <$> databaseFileOption <*> portOption
    startDesc = progDesc "Start Goals server"
    migrateDesc = progDesc "Setup Goals database"

databaseFileOption :: Parser Text
databaseFileOption =
  strOption
    (long "database-file" <> short 'd' <> metavar "PATH" <> value defaultDbPath <>
     showDefault <>
     help "Which database file to use")

portOption :: Parser Int
portOption =
  option
    auto
    (long "port" <> short 'p' <> metavar "PORT" <> value defaultPort <>
     showDefault <>
     help "Which port to run the server on")

main :: IO ()
main = join $ execParser opts
  where
    opts = info (parser <**> helper) desc
    desc =
      fullDesc <> progDesc "Manage projects one goal at a time" <>
      header "Goals"
