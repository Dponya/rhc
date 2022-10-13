{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.RHC.Internal.Inspector
  (
    injectMethods
  , generateTable
  ) where

import Data.Aeson ( fromJSON, Value, ToJSON(toJSON) )
import Data.ByteString.Lazy (ByteString, toStrict)
import Language.Haskell.TH
    ( listE, Q, ExpQ, Type, Dec, Name, reify, runIO, Info(VarI), dyn )
import Language.Haskell.TH.Syntax
    ( Q, Type, Dec, Name, reify, runIO, Info(VarI) )
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.RHC.Internal.Server (handleRequest, runWarp)
import Network.RHC.Internal.Orphans

import qualified Data.Text as T


injectMethods :: [(T.Text, Name)] -> Q [Dec]
injectMethods names =
  [d|
    serv :: Port -> IO ()
    serv port = runWarp port
      (
        (
          $(packSenderDomains names (generateTable names))
        ) :
          $(generateTable names)
      )
  |]

generateTable :: [(T.Text, Name)] -> ExpQ
generateTable names = listE $ fmap attachAction names
  where
    attachAction :: (T.Text, Name) -> ExpQ
    attachAction (s, n) = [|(s, $(dyn "executeDecoded") $ $(dyn . show $ n))|]

packSenderDomains :: [(T.Text, Name)] -> ExpQ -> ExpQ
packSenderDomains ns xs = [| $(tuple) |]
  where
    tuple = [| ("sendDomains", $(dyn "executeDecoded") $ $(dynamicApplied))|]
    arg :: ExpQ
    arg = let
        a = fmap (methodToExp . packMethodType) ns in listE a
    dynamicApplied = [| $(dyn "sendDomains") $(arg)|]

methodToExp :: Q (T.Text, Value) -> ExpQ
methodToExp m = m >>= (\x -> [|x|])

packMethodType :: (T.Text, Name) -> Q (T.Text, Value)
packMethodType (x, n) = do
  (VarI _ t _) <- reify n
  pure (x, toJSON t)
