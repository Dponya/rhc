{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Aeson (FromJSON, Object, decode)
import Data.Aeson.Types
import Network.RHC.Server (Methods, MethodName, Method, lookupMethod)

runWarpServer :: Methods -> Port -> IO ()
runWarpServer methods port =
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


-- performingProcess :: Builder -> Methods -> Maybe Method
{- performingProcess body methods = do
                                   let decodedBody :: FromJSON a => Maybe (RequestRHC a)
                                       decodedBody = decode . toLazyByteString $ body
                                   decoded <- decodedBody
                                   lookupMethod (method decoded) methods -}

performingProcess :: Builder -> Methods -> Maybe Method
performingProcess body methods = do
                                   decoded <- decode $ toLazyByteString body
                                   lookupMethod (method decoded) methods

data ParamsRHC = ArrayType [(Integer, *)] | ObjectType [(String, *)]

data RequestRHC = RequestRHC {
        resVersion :: String,
        method :: MethodName,
        params :: Maybe ParamsRHC,
        reqId :: Maybe String
}

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

instance FromJSON ParamsRHC where
        parseJSON _ = undefined