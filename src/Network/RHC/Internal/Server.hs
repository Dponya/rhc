{-# LANGUAGE OverloadedStrings #-}
module Network.RHC.Internal.Server where

import Network.Wai (
        responseBuilder,
        Application,
        Request,
        lazyRequestBody,
        strictRequestBody
        )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Data.ByteString.Builder (byteString, Builder)
import Data.ByteString.Lazy (toStrict, ByteString)
import Data.Aeson (FromJSON, Object, decode)
import Data.Aeson.Types

app :: Application
app req send = do
                let responseSender answer = send (responseBuilder status200 [] answer)
                body <- requestBodyReceiver req
                responseSender body

requestBodyReceiver :: Request -> IO Builder
requestBodyReceiver req = byteString . toStrict <$> lazyRequestBody req                        

data RequestRHC prm = RequestRHC {
        resVersion :: String,
        method :: String,
        params :: Maybe prm,  -- structure
        reqId :: Maybe String -- note: should change request type to sum type
                              -- between notification and ordinary request
}

data ResponseRHC res = Response {
        reqVersion :: String,
        result :: Maybe res, -- structure, determined structure by method
        resError :: Maybe String, -- must be an object
        resId :: Maybe String
}

instance FromJSON (RequestRHC a) where
        parseJSON (Object v) = 
                RequestRHC <$> v .: "jsonrpc"
                           <*> v .: "method"
                           <*> v .: "params"
                           <*> v .: "id"

-- test = run 3000 app