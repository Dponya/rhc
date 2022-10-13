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
import qualified Data.Aeson.KeyMap as KM 
import GHC.IO.Exception (IOErrorType(SystemError))
import qualified Data.Vector as V
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

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
    if KM.member "id" v
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

buildFromUser :: ErrorObject -> Res
buildFromUser obj = ResErrsWithoutId "2.0" obj

jsonToReq :: Value -> Maybe Req
jsonToReq (Object o) =
  do String ver <- KM.lookup "jsonrpc" o
     String mtd <- KM.lookup "method" o
     params <- KM.lookup "params" o
     case KM.member "id" o of
       False -> pure (Notif (T.unpack ver) (mtd) params)
       True -> do
        valId <- KM.lookup "id" o
        case fromJSON valId of
          Error s -> Nothing
          Success reqId -> pure (Req (T.unpack ver) (mtd) params reqId)
jsonToReq _ = Nothing

traverseBatchReq :: Value -> [Maybe Req]
traverseBatchReq val@(Array ar) = fmap jsonToReq (V.toList ar)
traverseBatchReq _ = [Nothing]

parseRequest :: ByteString -> RPC (Either [Maybe Req] Req)
parseRequest body = case eitherDecode @Value body of
  Left s -> throwM ParseError
  Right val@(Array v) -> pure $ Left $ traverseBatchReq val
  Right val@(Object v) -> case parseEither parseJSON val of
      Left s -> throwM InvalidRequest
      Right obj -> pure $ Right obj
  Right _ -> throwM ParseError

processRPC :: ByteString -> RPC Value
processRPC bs = do
  parsed <- (parseRequest bs)
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
