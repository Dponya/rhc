{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.RHC.Internal.Server where

import Data.Aeson (FromJSON, Object, decode, eitherDecode, encode)
import Data.Aeson.KeyMap (member)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.Wai.Handler.Warp (Port, run)

data Req
  = Notification
      { resVersion :: Text,
        method :: MethodName,
        params :: Value
      }
  | Request
      { resVersion :: Text,
        method :: MethodName,
        params :: Value,
        reqId :: Text
      }
  deriving (Show)

data Res res = Response
  { reqVersion :: Text,
    result :: Maybe res,
    resError :: Maybe String,
    resId :: Maybe String
  }

type MethodResult r = IO (Either Text r)

type MethodName = Text

type DecodeError = String

type Method a r = a -> MethodResult r

class RequestParse a where
  paramsParse :: MethodName -> Maybe (Value -> Parser a)

class RequestParse a => BuildResponse a r where
  performMethod :: a -> MethodResult r

decodeToReq :: ByteString -> Either DecodeError Req
decodeToReq = eitherDecode

toRPCRequest :: RequestParse a => ByteString -> Maybe a
toRPCRequest body = case eitherDecode body of
  Left s -> Nothing
  Right req -> case paramsParse (method req) of
    Nothing -> Nothing
    Just f -> parseMaybe f (params req)

responder :: (RequestParse a, BuildResponse a r) => Maybe a -> MethodResult r
responder (Just prm) = performMethod prm
responder _ = return $ Left "Nothing happened"

instance FromJSON Req where
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

runWarpServer :: forall a r. (BuildResponse a r) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [] answer)
          body <- toLazyByteString . byteString <$> getRequestBodyChunk req
          -- print $ toRPCRequest @b body
          result <- responder @a @r (toRPCRequest body)
          responseSender "done"
   in run port app
