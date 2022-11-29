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

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.Reader (MonadReader(ask))
import Control.Lens hiding ((.=))

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(toJSON)
  , object
  , decode
  , encode
  , (.=)
  , (.:)
  )
import Data.Aeson.Lens (_JSON, values)
import Data.Aeson.Types (Value(..), parseMaybe)
import Data.ByteString.Lazy (ByteString)

import Rhc.Server.Remote
  ( RemoteActionName
  , RPC
  , RemoteEnv(..)
  )
import Rhc.Server.Error
  ( ErrorExecutionCause(MethodNotFound)
  , ErrorParseCause(ParseError, InvalidRequest)
  )

import qualified Data.Aeson.KeyMap as KM

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

instance ToJSON a => ToJSON (ReqWithId a) where
  toJSON (ReqWithId {..}) =
    object
      [ "jsonrpc" .= ver
      , "method"  .= mtd
      , "params"  .= params
      , "id"      .= reqId
      ]

instance ToJSON a => ToJSON (Notif a) where
  toJSON (Notif {..}) =
    object
      [ "jsonrpc" .= ver
      , "method"  .= mtd
      , "params"  .= params
      ]

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

instance ToJSON (Req Value) where
  toJSON (FullReq req) = toJSON req
  toJSON (ReqNotif req) = toJSON req

handleRequest :: Req Value -> RPC (Req Value)
handleRequest = \case
  ReqNotif ntf@(Notif {..}) ->
    do (RemoteEnv table) <- ask
       f <- action mtd table
       val <- f (encode params)
       pure $ val <$ ReqNotif ntf
  FullReq full@(ReqWithId {..}) -> 
    do (RemoteEnv table) <- ask
       f <- action mtd table
       val <- f (encode params)
       pure $ val <$ FullReq full
  where
    action mtd table =
      maybe (throwM MethodNotFound)
            pure 
            (lookup mtd table)

jsonToReq :: Value -> Maybe (Req Value)
jsonToReq val = (full <&> FullReq) <|> (notif <&> ReqNotif)
  where
    full :: Maybe (ReqWithId Value)
    full = val ^? _JSON

    notif :: Maybe (Notif Value)
    notif = val ^? _JSON

batchReq :: Value -> [Maybe (Req Value)]
batchReq val = val ^.. values . to jsonToReq

parseRequest ::
  ByteString 
  -> RPC (Either [Maybe (Req Value)] (Req Value))
parseRequest bs = decode @Value bs
  & \case
      Nothing -> throwM ParseError
      Just val@(Array _) -> pure $ Left $ batchReq val
      Just val@(Object _) -> parseMaybe parseJSON val & \case
          Nothing -> throwM InvalidRequest
          Just obj -> pure $ Right obj
      Just _ -> throwM ParseError
