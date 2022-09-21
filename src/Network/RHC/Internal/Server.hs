{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Control.Exception hiding (Handler, catches)
import Control.Monad.Catch
import Data.Either
import Control.Monad.Reader (ReaderT(..))
import Network.RHC.Internal.RPCCommon
import Data.Aeson.KeyMap (member)
import GHC.IO.Exception (IOErrorType(SystemError))
import Control.Monad.IO.Class

type RPCVersion = String

data Req
  = Notif
      { reqVersion :: RPCVersion,
        method :: RemoteActionName,
        params :: Value
      }
  | Req
      { reqVersion :: RPCVersion,
        method :: RemoteActionName,
        params :: Value,
        reqId :: Integer
      } deriving Show

data Res
  = ResSuccess
      { resVersion :: RPCVersion,
        result :: Value,
        resId :: Integer
      }
  | ResErrsWithoutId
      { resVersion :: RPCVersion,
        resError :: ErrorObject
      }
  | ResErrsWithId
      {
        resVersion :: RPCVersion,
        resError :: ErrorObject,
        resId :: Integer
      }
  | ResSystemErrs
      { resVersion :: RPCVersion,
        resError :: ErrorObject
      }
  | ResVoid
      {
        void :: ()
      }

instance FromJSON Req where
  parseJSON (Object v) =
    if member "id" v
      then
        Req
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
          <*> v .: "id"
      else
        Notif
          <$> v .: "jsonrpc"
          <*> v .: "method"
          <*> v .: "params"
  parseJSON _ = mempty

instance ToJSON Res where
  toJSON (ResSuccess ver result rId) =
    object
      [ "jsonrpc" .= ver,
        "result" .= result,
        "id" .= rId
      ]
  toJSON (ResErrsWithoutId ver err) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err,
        "id" .= Null
      ]
  toJSON (ResErrsWithId ver err rId) =
    object
      [
        "jsonrpc" .= ver,
        "error" .= err,
        "id" .= rId
      ]
  toJSON (ResSystemErrs ver err) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err
      ]
  toJSON (ResVoid ()) = Null


executeDecoded :: forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM InvalidParams
    Right prm -> fun prm >>= pure . toJSON

handleRequest :: Req -> RPC ActionResponse
handleRequest req =
  do
    let action name table = maybe (throwM MethodNotFound) pure (lookup name table)
    (RemoteEnv table) <- ask
    f   <- action (method req) table
    f (encode . params $ req)

buildResponse :: Req -> ActionResponse -> Res
buildResponse Notif {} res = ResVoid ()
buildResponse (Req _ _ _ rId) result = ResSuccess "2.0" result rId

buildWithoutId :: ErrorCause -> Res
buildWithoutId e = ResErrsWithoutId ver (errObject e)
  where
    ver = "2.0"

buildWithId :: Integer -> ErrorCause -> Res
buildWithId rId e = ResErrsWithId ver (errObject e) rId
  where
    ver = "2.0"

buildFromUser :: Integer -> ErrorObject -> Res
buildFromUser rId e = ResErrsWithId ver e rId
  where
    ver = "2.0"

processRPC :: ByteString -> RemoteTable -> IO Res
processRPC body table = case eitherDecode @Req body of
  Left s -> pure . buildWithoutId $ ParseError
  Right req -> (performAction req env >>= pure . buildResponse req)
    `catches`
    [
      Handler (systemErrHandler (reqId req)),
      Handler (userDefinedHandler (reqId req)),
      Handler (parseErrHandler (reqId req))
    ]
  where
    env = RemoteEnv table
    performAction req = runReaderT (runRPC . handleRequest $ req)

    parseErrHandler :: Integer -> ErrorCause -> IO Res
    parseErrHandler _ InvalidRequest = pure $ buildWithoutId InvalidRequest
    parseErrHandler _ ParseError = pure $ buildWithoutId ParseError
    parseErrHandler rId e = pure $ buildWithId rId e

    systemErrHandler :: Integer -> IOException -> IO Res
    systemErrHandler rId _ = pure $ buildWithId rId InternalError

    userDefinedHandler :: Integer -> ErrorObject -> IO Res
    userDefinedHandler rId obj = pure $ buildFromUser rId obj 

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
