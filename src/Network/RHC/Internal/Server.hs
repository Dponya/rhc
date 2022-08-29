{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  deriving (Show)

data Res
  = ResSuccess
      { resVersion :: String,
        result :: Value,
        resId :: String
      }
  | ResError
      { resVersion :: String,
        resError :: [String],
        resId :: String
      }
  | ResSystemError
      { resVersion :: String,
        resError :: [String]
      }
  deriving (Show)

data ErrorObject
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError

-- type MethodResult r = IO (Either String r)

type MethodName = String

class RequestParse a where
  paramsParse :: MethodName -> Maybe (Value -> Parser a)

class RequestParse a => MethodPerform a where
  performMethod :: MethodName -> a -> Either ErrorObject Value

parseRequest ::
  forall a.
  (RequestParse a) =>
  ByteString ->
  Either String (Req, a)
parseRequest body = do
  req <- eitherDecode body ---- There's have skipped systemError handlings
  f <- case paramsParse @a (method req) of
    Nothing -> Left "Method not Found"
    Just f -> Right f
  prms <- parseEither f (params req)
  return (req, prms)

{- buildResponse :: MethodPerform a r => ParsedReq a -> r -> Res r
buildResponse
  ( ParsedRequest
      ver
      rId
      prm
      mtd
    )
  result = Response ver (Just result) Nothing rId -}

responder :: forall a r. MethodPerform a => (Req, a) -> IO Res
responder (req, prm) = case performMethod (method req) prm of
  Left s -> undefined
  Right va -> return $ ResSuccess (reqVersion req) va (reqId req)

{-

-}

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

runWarpServer :: forall a . (MethodPerform a) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [(hContentType, "application/json")] answer)
          p <- parseRequest @a <$> (toLazyByteString . byteString <$> getRequestBodyChunk req)
          case p of
            Left s -> undefined
            Right pr -> do
              result <- responder @a pr
              responseSender (byteString . toStrict . encode $ result)
          responseSender "done"
   in run port app
