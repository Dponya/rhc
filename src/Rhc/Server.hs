{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Rhc.Server
  ( executeDecoded
  , injectMethods
  , runWarp
  , RemoteAction
  , sendDomains
  , module Rhc.Server.Error
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT)
import Control.Monad.Catch (catches, Handler(Handler), MonadThrow(throwM))
import Control.Exception (IOException)
import Data.Aeson
    (eitherDecode, encode, FromJSON, Value, ToJSON(toJSON))
import Data.Aeson.Types (FromJSON, Value, ToJSON(toJSON))
import Data.ByteString.Builder (byteString, toLazyByteString, Builder)
import Data.ByteString.Lazy (ByteString, toStrict)
import GHC.IO.Exception (IOErrorType(SystemError))
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)
import Network.Wai
  ( Application,
    Request,
    getRequestBodyChunk,
    responseBuilder,
  )

import Rhc.Server.Domain (sendDomains)
import Rhc.Server.Error
  ( ErrorExecutionCause (..)
  , ErrorParseCause (..)
  , ErrorServCause (..)
  , ErrorObject (..)
  , ErrorCause (..)
  , errObject
  )
import Rhc.Server.Remote
  ( RemoteTable
  , RPC(runRPC)
  , RemoteEnv(RemoteEnv)
  )
import Rhc.Server.Request (Req(..), parseRequest, handleRequest)
import Rhc.Server.Response (Res(..), buildResponse, buildFromUser)
import Rhc.Server.Remote (ActionResponse, RemoteAction, injectMethods)


executeDecoded ::
  forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM InvalidParams
    Right prm -> fun prm >>= pure . toJSON

catchParseErrs :: ErrorParseCause -> IO Value
catchParseErrs e = pure $ toJSON build
  where
    build = ResErrsWithoutId "2.0" (errObject (ErrorParseCause e))

userDefinedHandler :: ErrorObject -> RPC Value
userDefinedHandler obj = pure $ toJSON $ buildFromUser obj

executionErrHandler :: ErrorExecutionCause -> Req -> RPC Value
executionErrHandler e req = pure $ toJSON $ buildWithId (ErrorExecutionCause e) req
  where
    buildWithId :: ErrorCause -> Req -> Res
    buildWithId e Notif {} = ResVoid ()
    buildWithId e (Req _ _ _ rId) = ResErrsWithId "2.0" (errObject e) rId

systemErrHandler :: IOException -> IO Value
systemErrHandler _ = pure $ toJSON $ buildWithoutId (ErrorServCause InternalError)
  where
    buildWithoutId :: ErrorCause -> Res
    buildWithoutId e = ResErrsWithoutId "2.0" (errObject e)

processRPC :: ByteString -> RPC Value
processRPC bs = do
  parsed <- parseRequest bs
  case parsed of
    Left reqs -> toJSON <$> batchPerform reqs
    Right req -> toJSON <$> singlePerform req

batchPerform :: [Maybe Req] -> RPC [Value]
batchPerform = traverse execute
  where
    execute (Just req) = (toJSON . buildResponse req <$> handleRequest req)
      `catches` [
        Handler userDefinedHandler,
        Handler $ flip executionErrHandler req
      ]
    execute Nothing = pure . toJSON $ build
    build = ResErrsWithoutId "2.0" (errObject (ErrorParseCause InvalidRequest))

singlePerform :: Req -> RPC Value
singlePerform req
  = (toJSON . buildResponse req <$> handleRequest req)
    `catches` [Handler $ flip executionErrHandler req]

mainThread :: ByteString -> RemoteTable -> IO Value
mainThread body table = executeRPC body env
  `catches` [Handler catchParseErrs, Handler systemErrHandler]
  where
    env = RemoteEnv table
    executeRPC bs = runReaderT (runRPC . processRPC $ bs)

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
      toBuilder = byteString . toStrict . encode @Value
      in
        do req <- parsedIORequest
           res <- mainThread req table
           responseSender . toBuilder $ res