{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.RHC.Internal.RPCCommon where

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Aeson (FromJSON, Object)
import Data.Aeson.Types
import Data.Aeson.KeyMap (member)
import Data.Void
import Network.RHC.Internal.RPCErrors
import GHC.Generics (Generic)

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

data DomainMethods = DomainMethods
  { domain :: String,
    methods :: [String]
  } deriving (Generic, Show)

instance ToJSON DomainMethods

instance FromJSON DomainMethods