module Main where

import Web.Scotty (scotty)
import Server (server)

main :: IO ()
main = scotty 3000 server
