{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.RHC.Internal.Server where

import Network.Wai (
        responseBuilder,
        Application,
        Request,
        lazyRequestBody,
        strictRequestBody
        )
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types (status200)
import Data.ByteString.Builder (byteString, Builder, toLazyByteString)
import Data.ByteString.Lazy (toStrict, ByteString)
import Data.Aeson (FromJSON, Object, decode, eitherDecode)
import Data.Aeson.Types
import Network.RHC.Server (Methods, MethodName, Method (Method), lookupMethod, MethodArguments)
import Data.Aeson.KeyMap (toList)

runWarpServer :: Port -> IO ()
runWarpServer port =
                let app :: Application
                    app req send =
                        do
                        let responseSender answer =
                                send (responseBuilder status200 [] answer)
                        body <- requestBodyReceiver req
                        responseSender body
                in run port app

requestBodyReceiver :: Request -> IO Builder
requestBodyReceiver req = byteString . toStrict <$> lazyRequestBody req

-- bodyRequestParse = parseMaybe (a -> Parser b) a

requestParsingProcess :: RequestParamsParser b => Builder -> Maybe b
requestParsingProcess body = do
                             requestRHC <- (decode :: ByteString -> Maybe RequestRHC)
                                        $ toLazyByteString body
                             paramsForParse <- params requestRHC
                             requestParamsParse  paramsForParse

class RequestParamsParser a where
        requestParamsParse :: Object -> Maybe a

data RequestRHC = RequestRHC {
        resVersion :: String,
        method :: MethodName,
        params :: Maybe Object,
        reqId :: Maybe String
} deriving Show

data ResponseRHC res = Response {
        reqVersion :: String,
        result :: Maybe res,
        resError :: Maybe String,
        resId :: Maybe String
}


instance FromJSON RequestRHC where
        parseJSON (Object v) =
                RequestRHC <$> v .: "jsonrpc"
                           <*> v .: "method"
                           <*> v .: "params"
                           <*> v .: "id"
        parseJSON _ = mempty