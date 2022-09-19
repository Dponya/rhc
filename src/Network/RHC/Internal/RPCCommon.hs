{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.RHC.Internal.RPCCommon where

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Aeson (FromJSON, Object)
import Data.Aeson.Types
import Data.Aeson.KeyMap (member)

newtype RemoteEnv = Env {
    table :: RemoteTable
  }

newtype RPC a = RPC { runRPC :: ReaderT RemoteEnv IO a }
  deriving (
      Monad, Applicative, Functor,
      MonadReader RemoteEnv, MonadIO,
      MonadThrow, MonadCatch
    )

type RemoteAction a b = a -> RPC b

type RemoteActionName = String

type RemoteTable = [(RemoteActionName, RemoteAction ByteString ByteString)]

data Req
  = Notif
      { reqVersion :: String,
        method :: RemoteActionName,
        params :: Value
      }
  | Req
      { reqVersion :: String,
        method :: RemoteActionName,
        params :: Value,
        reqId :: Integer
      } deriving Show

instance FromJSON Req where
  parseJSON (Object v) =
    if member "id" v
      then
        Req
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
          <*> v .: "id"
      else
        Notif
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
  parseJSON _ = mempty

{- data Res
  = ResSuccess
      { resVersion :: String,
        result :: Value,
        resId :: String
      }
  | ResError
      { resVersion :: String,
        resError :: ErrorObject,
        resId :: String
      }
  | ResSystemError
      { resVersion :: String,
        resError :: ErrorObject
      } -}
