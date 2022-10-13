{-# LANGUAGE OverloadedStrings #-}
module Network.RHC.Internal.Client where

import Language.Haskell.TH (ExpQ)
import Network.HTTP.Req
import Data.Aeson (object, (.=), Value)
import Control.Monad.IO.Class
import Network.RHC.Internal.RPCCommon (DomainMethods(..))

load :: [String] -> ExpQ
load = undefined

queryDomains :: [String] -> IO [DomainMethods]
queryDomains domains =
  runReq defaultHttpConfig $
    do let payload =
            object [
              "jsonrpc" .= ("2.0" :: String),
              "method" .= ("sendDomains" :: String),
              "params" .= domains,
              "id" .= (-100 :: Integer)
              ]
       r <- req
              POST
              (https "localhost")
              (ReqBodyJson payload)
              jsonResponse
              mempty
       liftIO $ print (responseBody r :: Value)
       pure [DomainMethods "example" undefined]
