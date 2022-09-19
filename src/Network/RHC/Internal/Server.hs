{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Network.RHC.Internal.Server where

import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Builder (byteString, toLazyByteString, Builder)
import Data.ByteString.Lazy
import Network.RHC.Internal.RPCErrors
import Network.RHC.Internal.RPCCommon (RPC (..), RemoteEnv (Env), RemoteAction, RemoteTable, Req (..))
import Control.Monad.RWS (MonadReader(..))
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Either
import Control.Monad.Reader (ReaderT(..))

executeDecoded :: forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ByteString
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM ParseError
    Right prm -> fun prm >>= pure . encode

handleRequest :: ByteString -> RPC ByteString
handleRequest body =
  do
    let unpack :: Either String Req -> RPC Req
        unpack = either (const (throwM ParseError)) pure
        action name table = maybe (throwM MethodNotFound) pure (lookup name table)
    (Env table) <- ask
    req <- unpack (eitherDecode body)
    f <- action (method req) table
    f (encode . params $ req)

runWarp :: Port -> RemoteTable -> IO ()
runWarp port table = run port app
  where
    app :: Application
    app req send = let
      responseSender answer =
        send (responseBuilder status200 [(hContentType, "application/json")] answer)
      parsedIORequest =
                  toLazyByteString .
                  byteString      <$>
                  getRequestBodyChunk req
      in do
          req <- parsedIORequest
          res <- runReaderT (runRPC (handleRequest req)) (Env table)
          responseSender (byteString . toStrict $ res)
