{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.RHC.Internal.RPCErrors(
    ErrorObject(..),
    ErrorCause(ErrorServCause),
    ErrorServCause(ServerError)
  )
import Network.Wai.Handler.Warp (Port)
import Network.RHC.Internal.Inspector (injectMethods)
import Network.RHC.Internal.RPCCommon (RemoteAction)
import Language.Haskell.TH(runQ)
import Control.Monad.IO.Class
import Network.RHC.Internal.Utils (executeDecoded, sendDomains)
import Control.Monad.Catch (MonadThrow(throwM))

doSome :: RemoteAction [Int] Int
doSome (x:x2:xs) = liftIO $ print x >> pure (x + x2)
doSome xs = liftIO $ print xs >> pure 0

doErr :: RemoteAction [Int] [Int]
doErr (x:x2:xs) = if x > x2
  then throwM (ErrorObject (ErrorServCause (ServerError (-32000))) "test message")
  else pure [x + x2]

injectMethods [
  ("example.doSome", 'doSome),
  ("example.doErr", 'doErr)
  ]

main :: IO ()
main = serv 3000