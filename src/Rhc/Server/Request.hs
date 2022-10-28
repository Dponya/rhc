{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Rhc.Server.Request
  ( Req(..)
  , parseRequest
  , handleRequest
  ) where

import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.Reader (MonadReader(ask))
import Data.Aeson
  ( FromJSON(..)
  , eitherDecode
  , Result(..)
  , encode
  , fromJSON
  , (.:)
  )
import Data.Aeson (ToJSON(toJSON), object, (.=))
import Data.Aeson.Types (Value(..), parseEither)
import Data.ByteString.Lazy (ByteString)
import Rhc.Server.Remote
  ( RemoteActionName
  , RPC
  , ActionResponse
  , RemoteEnv(..)
  )
import Rhc.Server.Error
    ( ErrorExecutionCause(MethodNotFound)
    , ErrorParseCause(ParseError, InvalidRequest)
    )

import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V


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
      } deriving stock Show

instance FromJSON Req where
  parseJSON (Object v) =
    if KM.member "id" v
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

instance ToJSON Req where
  toJSON (Req {..}) =
    object
      [ "jsonrpc" .= reqVersion
      , "method"  .= method
      , "params"  .= params
      , "id"      .= reqId
      ] 
  toJSON (Notif {..}) =
    object
      [ "jsonrpc" .= reqVersion
      , "method"  .= method
      , "params"  .= params
      ] 

handleRequest :: Req -> RPC ActionResponse
handleRequest req =
  do
    let action name table = maybe (throwM MethodNotFound) pure (lookup name table)
    (RemoteEnv table) <- ask
    f   <- action (method req) table
    f (encode . params $ req)

jsonToReq :: Value -> Maybe Req
jsonToReq (Object o) =
  do String ver <- KM.lookup "jsonrpc" o
     String mtd <- KM.lookup "method" o
     params <- KM.lookup "params" o
     case KM.member "id" o of
       False -> pure (Notif (T.unpack ver) mtd params)
       True -> do
        valId <- KM.lookup "id" o
        case fromJSON valId of
          Error _ -> Nothing
          Success reqId -> pure (Req (T.unpack ver) mtd params reqId)
jsonToReq _ = Nothing

traverseBatchReq :: Value -> [Maybe Req]
traverseBatchReq (Array ar) = fmap jsonToReq (V.toList ar)
traverseBatchReq _ = [Nothing]

parseRequest :: ByteString -> RPC (Either [Maybe Req] Req)
parseRequest body = case eitherDecode @Value body of
  Left _ -> throwM ParseError
  Right val@(Array _) -> pure $ Left $ traverseBatchReq val
  Right val@(Object _) -> case parseEither parseJSON val of
      Left _ -> throwM InvalidRequest
      Right obj -> pure $ Right obj
  Right _ -> throwM ParseError
