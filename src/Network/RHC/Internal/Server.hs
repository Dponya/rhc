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
import Data.Bifunctor
import Network.RHC.Internal.RPCErrors
import Network.RHC.Internal.RPCCommon
import Control.Monad.RWS (MonadReader(..))
import Control.Exception
import Control.Monad.Catch
import Data.Either
import Control.Monad.Reader (ReaderT(..))
import Network.RHC.Internal.RPCCommon

executeDecoded :: forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM ParseError
    Right prm -> fun prm >>= pure . toJSON

handleRequest :: ByteString -> RPC (Req, ActionResponse)
handleRequest body =
  do
    let unpack = either (const (throwM ParseError)) pure
        action name table = maybe (throwM MethodNotFound) pure (lookup name table)
    (RemoteEnv table) <- ask
    req <- unpack (eitherDecode body)
    f   <- action (method req) table
    res <- f (encode . params $ req)
    pure (req, res)

buildResponse :: (Req, ActionResponse) -> Res
buildResponse (Notif {}, _) = ResVoid ()
buildResponse (Req ver _ _ rId, result) = ResSuccess ver result rId

buildResponseErr :: ErrorCause -> IO Res
buildResponseErr e = undefined

processRPC :: ByteString -> RemoteTable -> IO Res
processRPC body table =
  (performAction env >>= pure . buildResponse)
    `Control.Monad.Catch.catches`
  [Control.Monad.Catch.Handler buildResponseErr]
  where
    env = RemoteEnv table
    performAction = runReaderT (runRPC . handleRequest $ body)

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
      toBuilder = byteString . toStrict . encode @Res
      in
        do req <- parsedIORequest
           res <- processRPC req table
           responseSender . toBuilder $ res
