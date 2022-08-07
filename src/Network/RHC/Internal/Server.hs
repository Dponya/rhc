{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Network.RHC.Internal.Server where

import Network.Wai (
        responseBuilder,
        Application,
        Request,
        lazyRequestBody,
        strictRequestBody, getRequestBodyChunk
        )
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types (status200)
import Data.ByteString.Builder (byteString, Builder, toLazyByteString)
import Data.Aeson (FromJSON, Object, decode, eitherDecode)
import Data.Aeson.Types
import Network.RHC.Server (Methods, MethodName, Method (Method), lookupMethod, MethodArguments)
import Data.Aeson.KeyMap (member)
import Control.Applicative (Alternative)
import Data.ByteString.Lazy

runWarpServer :: Port -> IO ()
runWarpServer port =
                let app :: Application
                    app req send =
                        do
                        let responseSender answer =
                                send (responseBuilder status200 [] answer)
                        body <- getRequestBodyChunk req
                        print $ decodeToReq $ (toLazyByteString . byteString) body
                        responseSender ""
                in run port app

requestBodyReceiver :: Request -> IO Builder
requestBodyReceiver req = byteString . toStrict <$> lazyRequestBody req

decodeToReq :: ByteString -> Maybe (Req Object)
decodeToReq = decode :: ByteString -> Maybe (Req Object)

toRPCRequest :: RequestParamsParser a => ByteString -> Maybe (Req (Maybe a))
toRPCRequest body = fmap (fmap requestParamsParse) (decodeToReq body)

class RequestParamsParser a where
        requestParamsParse :: Object -> Maybe a

class (RequestParamsParser a, Show a) => RequestDebugger a where
        printReq :: Req a -> IO ()

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

data ResponseRHC res = Response {
        reqVersion :: String,
        result :: Maybe res,
        resError :: Maybe String,
        resId :: Maybe String
}

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