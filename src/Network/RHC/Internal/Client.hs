{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Network.RHC.Internal.Client where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax
import Network.HTTP.Req
import Data.Aeson (object, (.=), Value, fromJSON)
import Control.Monad.IO.Class
import Network.RHC.Internal.RPCCommon (DomainMethods(..))
import Network.RHC.Internal.Orphans
import Network.RHC.Internal.Server
import Data.Aeson.Types (Result(..))
import Network.RHC.Internal.RPCErrors (ErrorObject)
import Control.Monad.Catch (MonadThrow(..))

load :: [String] -> ExpQ
load ds = undefined

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
              (http "localhost")
              (ReqBodyJson payload)
              jsonResponse
              (port 3000)
       case parseResponse (responseBody r :: Res) of
         Left eo -> throwM eo
         Right dms -> pure dms

parseResponse :: Res -> Either ErrorObject [DomainMethods]
parseResponse = \case
  ResSuccess s va n -> case fromJSON va of
    Error str -> undefined -- delete in future
    Success any -> Right any
  ResErrsWithoutId s eo -> Left eo
  ResErrsWithId s eo n -> Left eo
  ResSystemErrs s eo -> Left eo
  ResVoid x0 -> Left undefined -- delete in future
