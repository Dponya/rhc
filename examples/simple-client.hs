{-# LANGUAGE TemplateHaskell #-}

module Main where

import Rhc


load (CliConf 3000 "localhost" Http)
     ["example", "coffee"]

config :: CliConf
config = CliConf
    { cPort = 3000
    , cHost = "localhost"
    , cProtocol = Http
    }

main :: IO ()
main = remoteRunner config (makeCoffee "hello from client!") >>= print
