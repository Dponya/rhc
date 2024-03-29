{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Aeson
import Data.Aeson.Types
import Network.Wai.Handler.Warp (Port)
import Language.Haskell.TH(runQ)

import Rhc

doSome :: RemoteAction [Int] Int
doSome (x:x2:xs) = liftIO $ print x >> pure (x + x2)
doSome xs = liftIO $ print xs >> pure 0

doErr :: RemoteAction [Int] [Int]
doErr (x:x2:xs) = if x > x2
  then throwM (ErrorObject
                (ErrorServCause 
                  (ServerError (-32000)))
                "test message")
  else pure [x + x2]

makeCoffee :: RemoteAction String [String]
makeCoffee x = pure ["some", "pre-defined", "words"]

countCoffee :: RemoteAction String Int
countCoffee _ = pure 55

generate $ domain "example"
                ( method "doSome" 'doSome
               <> method "doErr" 'doErr )
             <>
           domain "coffee"
                ( method "makeCoffee" 'makeCoffee
               <> method "countCoffee" 'countCoffee )

main :: IO ()
main = serv 3000
