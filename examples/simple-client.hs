{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Reader (ReaderT (..))

import Rhc


load (CliConf 3000 "localhost" Http)
     ["example", "coffee"]

config :: CliConf
config = CliConf
    { cPort = 3000
    , cHost = "localhost"
    , cProtocol = Http
    }

remoteRunner :: RemoteCall a -> IO a
remoteRunner x = runReaderT (runCall x) config

main :: IO ()
main = remoteRunner (makeCoffee "hello from client!") >>= print
