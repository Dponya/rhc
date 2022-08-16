{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.RHC.Internal.Server where

import Control.Applicative (Alternative, (<|>))
import Data.Aeson (FromJSON, Object, decode, eitherDecode, encode)
import Data.Aeson.KeyMap (member, toList)
import Data.Aeson.Types
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy
import Network.HTTP.Types (status200)
import Network.RHC.Server (MethodName)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.Wai.Handler.Warp (Port, run)
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Vector as DV

data Req a
  = Notification
      { resVersion :: String,
        method :: MethodName,
        params :: a
      }
  | Request
      { resVersion :: String,
        method :: MethodName,
        params :: a,
        reqId :: String
      }
  deriving (Show, Functor)

data Res res = Response
  { reqVersion :: String,
    result :: Maybe res,
    resError :: Maybe String,
    resId :: Maybe String
  }

class FromJSON a => RequestParse a where
  paramsParse :: Value -> Maybe a
  paramsParse v = case fromJSON v of
    Error s -> Nothing
    Success any -> Just any

decodeToReq :: forall a. ByteString -> Maybe (Req Value)
decodeToReq = decode

toRPCRequest :: forall b. (RequestParse b) => ByteString -> Maybe b
toRPCRequest body = do
            req <- decodeToReq body
            paramsParse . params $ req

{- reqBind :: Applicative m => (a -> m b) -> Req -> m Req
reqBind f (Notification ver method prm) =
  Notification ver method
    <$> f prm
reqBind f (Request ver method prm reqId) =
  Request ver method
    <$> f prm
    <*> pure reqId -}

instance FromJSON (Req Value) where
  parseJSON (Object v) =
    if member "id" v
      then
        Request
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
          <*> v .: "id"
      else
        Notification
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
  parseJSON _ = mempty

-- Temporarily type scope with b variable to print result of parsing here
runWarpServer :: forall b. (Show b, RequestParse b) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [] answer)
          body <- toLazyByteString . byteString <$> getRequestBodyChunk req
          --print $ toRPCRequestM @_ @b body
          print $ toRPCRequest @b body
          responseSender ""
   in run port app
