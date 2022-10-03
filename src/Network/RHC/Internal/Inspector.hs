{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.RHC.Internal.Inspector where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, toStrict)
import Language.Haskell.TH
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.RHC.Internal.RPCErrors
import Network.RHC.Internal.RPCCommon
import Control.Monad.Catch
import Network.RHC.Internal.Server (handleRequest, runWarp)

injectMethods :: [(String, Name)] -> Q [Dec]
injectMethods names =
  [d|
    serv :: Port -> IO ()
    serv port = runWarp port
      (
        (
          $(packSenderDomains (generateTable names))
        ) :
          $(generateTable names)
      
      )
  |]

generateTable :: [(String, Name)] -> ExpQ
generateTable names = listE $ fmap attachAction names
  where
    attachAction :: (String, Name) -> ExpQ
    attachAction (s, n) = [|(s, $(dyn "executeDecoded") $ $(dyn . show $ n))|]

packSenderDomains :: ExpQ -> ExpQ
packSenderDomains xs = [| $(tuple) |]
  where
    tuple = [| ("sendDomains", $(dyn "executeDecoded") $ $(dynamicApplied))|]
    dynamicApplied = [| $(dyn "sendDomains") $ $(xs) |]
