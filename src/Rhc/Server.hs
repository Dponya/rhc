module Rhc.Server
  ( executeDecoded
  , runWarp
  , RemoteAction
  , Namespace
  , sendDomains
  , mainThread
  , singlePerform
  , batchPerform
  , domain
  , method
  , generate
  , module Rhc.Server.Error
  , module Rhc.Server.Request
  , module Rhc.Server.Response
  ) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Catch (catches, Handler(Handler), MonadThrow(throwM))
import Data.Aeson
    (eitherDecode, encode, FromJSON, Value, ToJSON(toJSON))
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy (ByteString, toStrict)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai
  ( Application,
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
  , ActionResponse
  , RemoteAction
  , Namespace
  , RemoteEnv(RemoteEnv)
  , domain
  , method
  , generate
  )
import Rhc.Server.Request (Req (..), Notif(..), ReqWithId(..), parseRequest, handleRequest)
import Rhc.Server.Response (Res(..), buildResponse, buildFromUser)
import Control.Exception.Base


executeDecoded ::
  forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left _ -> throwM InvalidParams
    Right prm -> fun prm >>= pure . toJSON

catchParseErrs :: ErrorParseCause -> IO Value
catchParseErrs e = pure $ toJSON build
  where
    build = ResErrsWithoutId "2.0" (errObject (ErrorParseCause e))

userDefinedHandler :: ErrorObject -> Req Value -> RPC Value
userDefinedHandler obj req = pure $ toJSON $ build obj req
  where
    build :: ErrorObject -> Req Value -> Res
    build _ (ReqNotif _) = ResVoid ()
    build errObj (FullReq (ReqWithId _ _ _ rId)) = ResErrsWithId "2.0" errObj rId

executionErrHandler :: ErrorExecutionCause -> Req Value -> RPC Value
executionErrHandler e req = pure $ toJSON $ buildWithId (ErrorExecutionCause e) req
  where
    buildWithId :: ErrorCause -> Req Value -> Res
    buildWithId _ (ReqNotif _) = ResVoid ()
    buildWithId c (FullReq (ReqWithId _ _ _ rId)) = ResErrsWithId "2.0" (errObject c) rId

systemErrHandler :: SomeException -> IO Value
systemErrHandler _ = pure $ toJSON $ buildWithoutId (ErrorServCause InternalError)
  where
    buildWithoutId :: ErrorCause -> Res
    buildWithoutId e = ResErrsWithoutId "2.0" (errObject e)

batchPerform :: [Maybe (Req Value)] -> RPC [Value]
batchPerform = traverse execute
  where
    execute (Just req) = (toJSON . buildResponse <$> handleRequest req)
      `catches` [
        Handler $ flip userDefinedHandler req,
        Handler $ flip executionErrHandler req
      ]
    execute Nothing = pure . toJSON $ build
    build = ResErrsWithoutId "2.0" (errObject (ErrorParseCause InvalidRequest))

singlePerform :: Req Value -> RPC Value
singlePerform req
  = (toJSON . buildResponse <$> handleRequest req)
    `catches`
      [ Handler $ flip executionErrHandler req
      , Handler $ flip userDefinedHandler req
      ]

processRPC :: ByteString -> RPC Value
processRPC bs = do
  parsed <- parseRequest bs
  case parsed of
    Left reqs -> toJSON <$> batchPerform reqs
    Right req -> toJSON <$> singlePerform req

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
        do r <- parsedIORequest
           res <- mainThread r table
           responseSender . toBuilder $ res
