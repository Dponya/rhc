{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)

data Req
  = Notif
      { reqVersion :: String,
        method :: MethodName,
        params :: Value
      }
  | Req
      { reqVersion :: String,
        method :: MethodName,
        params :: Value,
        reqId :: String
      }

data Res
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
      }

data ErrorCause
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError Integer

data ErrorObject
  = ErrorObject
      { code :: ErrorCause,
        message :: String
      }
  | ErrorObjectAdditional
      { code :: ErrorCause,
        message :: String,
        additional :: Value
      }

type MethodName = String

class RequestParse a where
  paramsParse :: MethodName -> Maybe (Value -> Parser a)

class RequestParse a => MethodPerform a where
  performMethod :: MethodName -> a -> IO (Either ErrorObject Value)

parseReqBody :: ByteString -> Either ErrorObject Req
parseReqBody body =
  case eitherDecode body of
    Left s -> Left $ ErrorObject ParseError s
    Right r -> Right r

getPrmParser ::
  forall a.
  RequestParse a =>
  Req ->
  Either ErrorObject (Value -> Parser a)
getPrmParser req = case paramsParse @a (method req) of
  Nothing -> Left $ ErrorObject MethodNotFound ("Method " ++ method req ++ " Not Found")
  Just f -> Right f

parseReqPrm :: (Value -> Parser a) -> Value -> Either ErrorObject a
parseReqPrm f prm = case parseEither f prm of
  Left s -> Left $ ErrorObject InvalidParams s
  Right a -> Right a

parseRequest ::
  forall a.
  RequestParse a =>
  ByteString ->
  Either ErrorObject (Req, a)
parseRequest body = do
  req <- parseReqBody body
  f <- getPrmParser req
  prms <- parseReqPrm f (params req)
  return (req, prms)

responder :: forall a r. MethodPerform a => (Req, a) -> IO (Maybe Res)
responder (Notif ver mtd _, prm) = performMethod mtd prm >> return Nothing
responder (Req ver mtd _ rId, prm) =
  do
    res <- performMethod mtd prm
    case res of
      Left eo -> return . Just $ ResError ver eo rId
      Right va -> return . Just $ ResSuccess ver va rId

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

instance ToJSON ErrorCause where
  toJSON ParseError = toJSON @Integer (-32700)
  toJSON InvalidRequest = toJSON @Integer (-32600)
  toJSON MethodNotFound = toJSON @Integer (-32601)
  toJSON InvalidParams = toJSON @Integer (-32602)
  toJSON InternalError = toJSON @Integer (-32603)
  toJSON (ServerError num) = toJSON num

instance ToJSON ErrorObject where
  toJSON (ErrorObject code msg) =
    object
      [ "code" .= code,
        "message" .= msg
      ]
  toJSON
    (ErrorObjectAdditional code msg additional) =
      object
        [ "code" .= code,
          "message" .= msg,
          "data" .= additional
        ]

runWarpServer :: forall a. (MethodPerform a) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [(hContentType, "application/json")] answer)
          p <- parseRequest @a <$> (toLazyByteString . byteString <$> getRequestBodyChunk req)
          case p of
            Left s -> responseSender (byteString . toStrict . encode $ s)
            Right pr -> do
              result <- responder @a pr
              case result of
                Nothing -> responseSender (byteString . toStrict . encode $ toJSON ())
                Just res -> responseSender (byteString . toStrict . encode $ res)
   in run port app
