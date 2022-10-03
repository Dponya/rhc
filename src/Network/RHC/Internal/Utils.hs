{-# LANGUAGE RankNTypes #-}
module Network.RHC.Internal.Utils where

import Data.Aeson
import Data.ByteString.Lazy
import Network.RHC.Internal.RPCCommon
import Control.Monad.Catch
import Network.RHC.Internal.RPCErrors

executeDecoded :: forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM InvalidParams
    Right prm -> fun prm >>= pure . toJSON

sendDomains :: RemoteTable -> RemoteAction [String] DomainMethods
sendDomains table = undefined
