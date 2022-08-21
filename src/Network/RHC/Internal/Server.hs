{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.RHC.Internal.Server where

import Data.Aeson (FromJSON, Object, decode, eitherDecode, encode)
import Data.Aeson.KeyMap (member, toList)
import Data.Aeson.Types
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy
import Network.HTTP.Types (status200)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.Wai.Handler.Warp (Port, run)
import Data.Aeson.Text (encodeToTextBuilder)
import GHC.TypeLits
import Data.Kind (Type)

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

type MethodResult = IO (Either String String)

type MethodName = String

data Method = forall a. FromJSON a => Method { runMethod :: a -> MethodResult }

type Methods = [(MethodName, Method)]

class FromJSON a => UnCarriedMethod f a where
  carryToProcedure :: f -> a -> MethodResult

class FromJSON a => RequestParse a where
  paramsParse :: Value -> Either String a
  paramsParse v = case fromJSON v of
    Error s ->  Left s
    Success p -> Right p

initMethod :: UnCarriedMethod fn a => fn -> a -> MethodResult
initMethod = carryToProcedure

decodeToReq :: forall a. ByteString -> Either String (Req Value)
decodeToReq = eitherDecode

toRPCRequest :: forall b. RequestParse b => ByteString -> Either String (Req b)
toRPCRequest body = do
            req <- decodeToReq body
            parsedPrms <- paramsParse . params $ req
            return req { params = parsedPrms }

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

instance (FromJSON a, FromJSON b) => UnCarriedMethod (a -> IO b) a where
  carryToProcedure f a = do
                  _ <- f a
                  return $ Right "good"

instance UnCarriedMethod b [a] => UnCarriedMethod (a -> b) [a] where
  carryToProcedure f (x:xs) = carryToProcedure (f x) xs
  carryToProcedure _ _ = return $ Left "too few arguments"

instance FromJSON a => UnCarriedMethod (IO b) [a] where
  carryToProcedure f [] = do
                  _ <- f
                  return $ Right "good"
  carryToProcedure _ _ = return $ Left "too many arguments"

-- Temporarily type scope with b variable to print result of parsing here
runWarpServer :: forall b. (Show b, RequestParse b) => Port -> IO ()
runWarpServer port =
  let app :: Application
      app req send =
        do
          let responseSender answer =
                send (responseBuilder status200 [] answer)
          body <- toLazyByteString . byteString <$> getRequestBodyChunk req
          -- print $ toRPCRequest @b body
          responseSender "done"
   in run port app

{-
TODO:
2) Take over RequestParse from context of runWarpServer and try to set it to method defining func
  examlpe: method @Ruler \x -> print $ incrementAge x
3) Take a result of producing result from method calling and build response
-}
