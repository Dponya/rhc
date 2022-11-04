{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Rhc.Server.Request
  ( Req(..)
  , ReqWithId(..)
  , Notif(..)
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

data Req a
  = ReqNotif (Notif a) 
  | FullReq (ReqWithId a)
  deriving stock (Functor, Show)

data Notif a = Notif
  { ver :: String
  , mtd :: RemoteActionName
  , params :: a
  } deriving stock (Functor, Show)

data ReqWithId a = ReqWithId
  { ver :: String
  , mtd :: RemoteActionName
  , params :: a
  , reqId :: Integer
  } deriving stock (Functor, Show)

{- data Req
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
      } deriving stock Show -}

instance FromJSON (ReqWithId Value) where
  parseJSON (Object v) = ReqWithId
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
          <*> v .: "id"
  parseJSON _ = mempty

instance FromJSON (Notif Value) where
  parseJSON (Object v) = Notif
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
  parseJSON _ = mempty

instance FromJSON (Req Value) where
  parseJSON jsn@(Object v) =
    if KM.member "id" v
    then FullReq <$> parseJSON @(ReqWithId Value) jsn
    else ReqNotif <$> parseJSON @(Notif Value) jsn
  parseJSON _ = mempty

{- instance FromJSON (Req Value) where
  parseJSON (Object v) =
    if KM.member "id" v
      then FullReq $
        ReqWithId
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
          <*> v .: "id"
      else ReqNotif $
        Notif
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
  parseJSON _ = mempty -}

instance ToJSON (Req Value) where
  toJSON (FullReq (ReqWithId {..})) =
    object
      [ "jsonrpc" .= ver
      , "method"  .= mtd
      , "params"  .= params
      , "id"      .= reqId
      ] 
  toJSON (ReqNotif (Notif {..})) =
    object
      [ "jsonrpc" .= ver
      , "method"  .= mtd
      , "params"  .= params
      ] 

{- handleRequest :: Req Value -> RPC ActionResponse
handleRequest req =
  do
    let action name table = maybe (throwM MethodNotFound) pure (lookup name table)
    (RemoteEnv table) <- ask
    f   <- action (method req) table
    f (encode . params $ req) -}

{- handleRequest :: Req Value -> RPC ActionResponse
handleRequest = undefined -}

handleRequest :: Req Value -> RPC (Req Value)
handleRequest = \case
  ReqNotif ntf@(Notif {..}) ->
    do (RemoteEnv table) <- ask
       f <- action mtd table
       val <- f (encode params)
       pure $ const val <$> ReqNotif ntf
  FullReq full@(ReqWithId {..}) -> 
    do (RemoteEnv table) <- ask
       f <- action mtd table
       val <- f (encode params)
       pure $ const val <$> FullReq full
  where
    action mtd table =
      maybe (throwM MethodNotFound)
            pure 
            (lookup mtd table)

jsonToReq :: Value -> Maybe (Req Value)
jsonToReq (Object o) =
  do String ver <- KM.lookup "jsonrpc" o
     String mtd <- KM.lookup "method" o
     params <- KM.lookup "params" o
     case KM.member "id" o of
       False -> pure (ReqNotif $ Notif (T.unpack ver) mtd params)
       True -> do
        valId <- KM.lookup "id" o
        case fromJSON valId of
          Error _ -> Nothing
          Success reqId -> pure (FullReq $ ReqWithId (T.unpack ver) mtd params reqId)
jsonToReq _ = Nothing

traverseBatchReq :: Value -> [Maybe (Req Value)]
traverseBatchReq (Array ar) = fmap jsonToReq (V.toList ar)
traverseBatchReq _ = [Nothing]

parseRequest :: ByteString -> RPC (Either [Maybe (Req Value)] (Req Value))
parseRequest body = case eitherDecode @Value body of
  Left _ -> throwM ParseError
  Right val@(Array _) -> pure $ Left $ traverseBatchReq val
  Right val@(Object _) -> case parseEither parseJSON val of
      Left _ -> throwM InvalidRequest
      Right obj -> pure $ Right obj
  Right _ -> throwM ParseError
