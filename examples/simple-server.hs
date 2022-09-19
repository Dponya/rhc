{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.RHC.Internal.RPCErrors(
    ErrorObject(..),
    ErrorCause(ServerError)
  )
import Network.Wai.Handler.Warp (Port)
import Network.RHC.Internal.Inspector (injectMethods)
import Network.RHC.Internal.RPCCommon (RemoteAction)
import Language.Haskell.TH(runQ)
import Control.Monad.IO.Class
import Network.RHC.Internal.Server (executeDecoded)

doSome :: RemoteAction [Int] Int
doSome (x:x2:xs) = liftIO $ print x >> pure (x + x2)
doSome xs = liftIO $ print xs >> pure 0 

injectMethods [
  ("example.doSome", 'doSome)
  ]

main :: IO ()
main = serv 3000