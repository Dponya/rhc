{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.RHC.Internal.RPCCommon where

import Data.ByteString.Lazy (ByteString)
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Aeson (FromJSON, Object)
import Data.Aeson.Types
import Data.Aeson.KeyMap (member)
import Network.RHC.Internal.RPCErrors
import Data.Void

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

data Res
  = ResSuccess
      { resVersion :: String,
        result :: Value,
        resId :: Integer
      }
  | ResError
      { resVersion :: String,
        resError :: ErrorObject,
        resId :: Integer
      }
  | ResSystemError
      { resVersion :: String,
        resError :: ErrorObject
      }
  | ResVoid
      {
        void :: ()
      }

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

instance ToJSON Res where
  toJSON (ResSuccess ver result rId) =
    object
      [ "jsonrpc" .= ver,
        "result" .= result,
        "id" .= rId
      ]
  toJSON (ResError ver err rId) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err,
        "id" .= rId
      ]
  toJSON (ResSystemError ver err) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err
      ]
  toJSON (ResVoid ()) = object []
