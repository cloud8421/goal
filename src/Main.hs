module Main where

import Lib
import Network.Wai.Handler.Warp (run)

defaultPort :: Int
defaultPort = 8080

main :: IO ()
main = run defaultPort app
