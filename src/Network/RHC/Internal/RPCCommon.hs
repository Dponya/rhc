{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.RHC.Internal.RPCCommon where

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Aeson (FromJSON, Object)
import Data.Aeson.Types
import Data.Aeson.KeyMap (member)
import Data.Void
import Network.RHC.Internal.RPCErrors

newtype RemoteEnv = RemoteEnv {
    table :: RemoteTable
  }

newtype RPC a = RPC { runRPC :: ReaderT RemoteEnv IO a }
  deriving (
      Monad, Applicative, Functor,
      MonadReader RemoteEnv, MonadIO,
      MonadThrow, MonadCatch
    )

type ActionResponse = Value

type RemoteAction a b = a -> RPC b

type RemoteActionName = String

type RemoteTable = [(RemoteActionName, RemoteAction ByteString ActionResponse)]
