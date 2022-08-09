{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.RHC.Internal.Server where

import Network.Wai (
        responseBuilder,
        Application,
        Request,
        getRequestBodyChunk
        )
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types (status200)
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.Aeson (FromJSON, Object, decode)
import Data.Aeson.Types
import Network.RHC.Server (MethodName)
import Data.Aeson.KeyMap (member)
import Control.Applicative (Alternative)
import Data.ByteString.Lazy

data Req prm = Notification {
                resVersion :: String,
                method :: MethodName,
                params :: prm
        } | Request {
                resVersion :: String,
                method :: MethodName,
                params :: prm,
                reqId :: String
        } deriving (Show, Functor)

data Res res = Response {
        reqVersion :: String,
        result :: Maybe res,
        resError :: Maybe String,
        resId :: Maybe String
}

class RequestParamsParser a where
        requestParamsParse :: Object -> Maybe a

decodeToReq :: ByteString -> Maybe (Req Object)
decodeToReq = decode :: ByteString -> Maybe (Req Object)

toRPCRequest :: RequestParamsParser b => ByteString -> Maybe (Req b)
toRPCRequest body = do
                req <- decodeToReq body
                reqBind requestParamsParse req

reqBind :: Applicative m => (a -> m b) -> Req a -> m (Req b)
reqBind f (Notification ver method prm) = Notification ver method
                                                <$> f prm
reqBind f (Request ver method prm reqId) = Request ver method
                                                <$> f prm
                                                <*> pure reqId

printRPCRequest :: (Show b, RequestParamsParser b) => Maybe (Req b) -> IO ()
printRPCRequest (Just req) = print req
printRPCRequest Nothing = print "Parsing problem"

instance FromJSON (Req Object) where
        parseJSON (Object v) = if member "id" v
                                then Request
                                  <$> v .: "jsonrpc"
                                  <*> v .: "method"
                                  <*> v .: "params"
                                  <*> v .: "id"
                                else Notification
                                  <$> v .: "jsonrpc"
                                  <*> v .: "method"
                                  <*> v .: "params"
        parseJSON _ = mempty

-- Temporarily type scope with b variable to print result of parsing here
runWarpServer :: forall b . (Show b, RequestParamsParser b) => Port -> IO ()
runWarpServer port =
                let app :: Application
                    app req send =
                        do
                        let responseSender answer =
                                send (responseBuilder status200 [] answer)
                        body <- toLazyByteString . byteString <$> getRequestBodyChunk req
                        printRPCRequest $ toRPCRequest @b body
                        responseSender ""
                in run port app
