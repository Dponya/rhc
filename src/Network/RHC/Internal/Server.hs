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
import Data.ByteString.Lazy (toStrict)

{- app :: Application
app req send = send (responseBuilder status200 []) <$> requestBodyReceiver req -}
-- app req send = send $ responseBuilder status200 [] $ reqPath req

requestBodyReceiver :: Request -> IO Builder
requestBodyReceiver req = byteString . toStrict <$> lazyRequestBody req 
