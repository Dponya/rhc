{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.RHC.Internal.Server where

import Control.Monad
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

{- data Res res = Response
  { reqVersion :: Text,
    result :: Maybe res,
    resError :: Maybe String,
    resId :: Maybe String
  }
-}

data ParsedRequest a = ParsedRequest
  { requestId :: Maybe Text,
    parsedParams :: a,
    methodName :: MethodName
  }

type MethodResult r = IO (Either Text r)

type MethodName = Text

class RequestParse a where
  paramsParse :: MethodName -> Maybe (Value -> Parser a)

class RequestParse a => MethodPerform a r where
  performMethod :: MethodName -> a -> MethodResult r

toParsedRequest :: Req -> a -> ParsedRequest a
toParsedRequest (Notification { method }) prm =
  ParsedRequest Nothing prm method
toParsedRequest (Request { reqId, method }) prm =
  ParsedRequest (Just reqId) prm method

parseRequest ::
  forall a.
  (RequestParse a, FromJSON a) =>
  ByteString ->
  Either String (ParsedRequest a)
parseRequest body = do
  req <- eitherDecode body
  f <- case paramsParse @a (method req) of
    Nothing -> Left "Method Not Found"
    Just f -> Right f
  prms <- parseEither f (params req)
  return (toParsedRequest req prms)

responder :: MethodPerform a r => ParsedRequest a -> MethodResult r
responder (ParsedRequest { requestId, parsedParams, methodName }) =
  performMethod methodName parsedParams

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

runWarpServer :: forall a r. (MethodPerform a r) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [] answer)
          body <- toLazyByteString . byteString <$> getRequestBodyChunk req
          -- print $ toRPCRequest @b body
          --result <- responder @a @r (parseRequest body)
          responseSender "done"
   in run port app
