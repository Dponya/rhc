{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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
import Network.RHC.Server (Methods, MethodName, Method (Method), lookupMethod)
import Data.Aeson.KeyMap (toList)

runWarpServer :: Methods -> Port -> IO ()
runWarpServer methods port =
                let app :: Application
                    app req send =
                        do
                        let responseSender answer =
                                send (responseBuilder status200 [] answer)
                        body <- requestBodyReceiver req
                        print $ (eitherDecode :: ByteString -> Either String RequestRHC) 
                                $ toLazyByteString body
                        responseSender body
                in run port app

requestBodyReceiver :: Request -> IO Builder
requestBodyReceiver req = byteString . toStrict <$> lazyRequestBody req

performingProcess :: Builder -> Methods -> Maybe Method
performingProcess body methods = do
                                   decoded <- decode $ toLazyByteString body
                                   lookupMethod (method decoded) methods

data ParamsRHC = ArrayParams [(Key, Value)] deriving Show

data RequestRHC = RequestRHC {
        resVersion :: String,
        method :: MethodName,
        params :: Maybe ParamsRHC,
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

instance FromJSON ParamsRHC where
        parseJSON (Object v) = return $ ArrayParams $ Data.Aeson.KeyMap.toList v
        parseJSON _ = undefined