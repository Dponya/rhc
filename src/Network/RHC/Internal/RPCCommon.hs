{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.RHC.Internal.RPCCommon where

import Data.Aeson (FromJSON, Object, Value, ToJSON(..))
import Data.Aeson.KeyMap (member)
import Data.ByteString.Lazy (ByteString)
import Data.Text(Text)
import Control.Monad.Reader
    ( MonadIO, MonadReader, ReaderT(ReaderT) )
import Control.Monad.Catch ( MonadCatch, MonadThrow )
import GHC.Generics (Generic)

import Network.RHC.Internal.Orphans

newtype RemoteEnv = RemoteEnv {
    table :: RemoteTable
  }

newtype RPC a = RPC { runRPC :: ReaderT RemoteEnv IO a }
  deriving newtype (
      Monad, Applicative, Functor,
      MonadReader RemoteEnv, MonadIO,
      MonadThrow, MonadCatch
    )

type ActionResponse = Value

type RemoteAction a b = a -> RPC b

type RemoteActionName = Text

type RemoteActionDomain = Text

type RemoteTable = [(
    RemoteActionName,
    RemoteAction ByteString ActionResponse
  )]

data MethodInfo ty = MethodInfo
  { methodName :: Text
  , methodType :: ty
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DomainMethods ty = DomainMethods
  { domain :: Text
  , methods :: [MethodInfo ty]
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq (DomainMethods a) where
  (DomainMethods d _) == (DomainMethods d1 _) = d == d1
