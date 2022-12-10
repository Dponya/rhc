{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Rhc.Client
  ( CliConf (..)
  , CliProtocol (..)
  , RemoteCall (..)
  , load
  , remoteRunner
  , remoteNotifier
  , specify
  , rand
  ) where


import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, object, (.=), toJSON)
import Data.Aeson.Types (Result (..))
import Data.Function
import Data.Proxy
import System.Random.Stateful
import Language.Haskell.TH (clause, funD, normalB)
import Language.Haskell.TH.Syntax
    (mkName, Q, Type(ConT, ForallT, ArrowT, AppT), Dec(SigD), runIO, Exp)
import Network.HTTP.Req
    ( JsonResponse
    , defaultHttpConfig
    , http
    , https
    , jsonResponse
    , port
    , req
    , responseBody
    , runReq
    , POST(POST)
    , ReqBodyJson(ReqBodyJson)
    )

import Rhc.Server.Domain (DomainMethods(..), MethodInfo(..))
import Rhc.Server.Error (ErrorObject)
import Rhc.Server.Response
    ( Res 
      ( ResSuccess
      , ResErrsWithoutId
      , ResErrsWithId
      , ResSystemErrs
      , ResVoid
      )
    )

import qualified Data.Text as T
import qualified Network.HTTP.Req as R


data CliProtocol = Https | Http | Websocket

data CliConf = CliConf
  { cPort :: Int,
    cHost :: String,
    cProtocol :: CliProtocol
  }

data RequestOptions =
  Options ReqId CliConf

newtype RemoteCall a = RemoteCall
  { unRemoteCall :: ReaderT RequestOptions IO a }
  deriving newtype
    ( Monad
    , Applicative
    , Functor
    , MonadReader RequestOptions
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

data ClientErrs = CliSysErr String
  deriving stock Show
  deriving anyclass Exception

type RemoteCall' a = RemoteCall (Either () a)

data ReqIdOption =
    Specify Integer
  | Rand

specify :: Integer -> ReqIdOption
specify = Specify

rand :: ReqIdOption
rand = Rand

randReqIdGen :: IO Integer
randReqIdGen = impureGen
  where
    range = uniformR (-32000 :: Integer, -32099 :: Integer)
    impureGen = applyAtomicGen range globalStdGen

remoteRunner :: CliConf -> ReqIdOption -> RemoteCall' a -> IO a
remoteRunner conf rOption fn
  = rOption &
      \case
        Specify rId -> (runReaderT (unRemoteCall fn) (options rId conf))
          >>= unwrap
        Rand -> randReqIdGen >>= \rId ->
          (runReaderT (unRemoteCall fn) (options rId conf)) >>= unwrap
    where
      unwrap :: Either () a -> IO a
      unwrap (Right a) = pure a
      unwrap _ = throwM $ CliSysErr "unwrapping response failed"
      options :: Integer -> CliConf -> RequestOptions
      options rId conf = Options (Just rId) conf

remoteNotifier :: CliConf -> RemoteCall' a -> IO ()
remoteNotifier conf fn =
  runReaderT (unRemoteCall fn) options >>= unwrap
  where
    unwrap :: Either () a -> IO ()
    unwrap (Left ()) = pure ()
    unwrap _ = throwM $ CliSysErr "unwrapping response failed"
    options :: RequestOptions
    options = Options Nothing conf

load :: CliConf -> [String] -> Q [Dec]
load conf ds = runIO (queryDomains conf ds) >>=
  \xs -> runIO (mapM rebuildDms xs) >>= traverseDms

queryDomains :: CliConf -> [String] -> IO [DomainMethods Type]
queryDomains conf domains =
  runRequest "sendDomains" (-100) domains conf

rebuildSig :: Type -> Either String Type
rebuildSig = rebuild
  where
    rebuild (ForallT vrs ctx t) = fmap (ForallT vrs ctx) (rebuild t)
    rebuild (AppT (AppT _ farg) sarg) = Right
      (AppT (AppT ArrowT farg) (AppT monadTy (sarg)))
    rebuild _ = Left "not implemented"

    monadTy = ConT ''RemoteCall'

rebuildDms :: DomainMethods Type -> IO (DomainMethods Type)
rebuildDms (DomainMethods dm ms) = DomainMethods dm <$> mapM unpackMs ms
  where
    unpackMs (MethodInfo n ty) = MethodInfo n <$> (case rebuildSig ty of
      Left s -> throwM $ CliSysErr s
      Right ty' -> pure ty')

traverseDms :: [DomainMethods Type] -> Q [Dec]
traverseDms ds = fmap (mconcat . mconcat) (declarations ds)
  where
    declarations :: [DomainMethods Type] -> Q [[[Dec]]]
    declarations = mapM fnFmap

    fnFmap :: DomainMethods Type -> Q [[Dec]]
    fnFmap x = mapM (declare (domain x)) . methods $ x

    declare :: T.Text -> MethodInfo Type -> Q [Dec]
    declare dm mtd = declareServMethods 
      (T.unpack dm)
      (T.unpack $ methodName mtd)
      (methodType mtd)

declareServMethods :: String -> String -> Type -> Q [Dec]
declareServMethods dm nm ty = do
  fnImpl <- funD fnName [clause [] (normalB fnBody) []]
  pure [typeSig, fnImpl]
  where
    fnBody = generateFnBody dm nm
    fnName = mkName nm
    typeSig = SigD fnName ty

generateFnBody :: String -> String -> Q Exp
generateFnBody domainName fnName =
  [|\arg ->
      do (Options reqType conf) <- ask
         reqType & \case
          Just rId -> Right <$> runRequest
            (domainName <> "." <> fnName)
            rId
            arg
            conf
          Nothing -> Left <$> runNotif
            (domainName <> "." <> fnName)
            arg
            conf
  |]

parseRemoteResult :: Res -> Either ErrorObject Value
parseRemoteResult = \case
  ResSuccess _ va _ -> Right va
  ResErrsWithoutId _ eo -> Left eo
  ResErrsWithId _ eo _ -> Left eo
  ResSystemErrs _ eo -> Left eo
  ResVoid _ -> Right $ toJSON ()

type FullDomain = String
type ReqId = Maybe Integer

httpRun :: Proxy ('Http)
httpRun = Proxy

httpsRun :: Proxy ('Https)
httpsRun = Proxy

websocketRun :: Proxy ('Websocket)
websocketRun = Proxy

runRequest :: (MonadIO m, ToJSON arg, FromJSON b) => 
  FullDomain ->
  Integer ->
  arg ->
  CliConf -> m b
runRequest fullDomain reqId arg conf@(CliConf {..}) =
  cProtocol & \case
    Http -> run httpRun
      fullDomain reqId arg conf
    Https -> run httpsRun
      fullDomain reqId arg conf
    Websocket -> undefined

runNotif :: (ToJSON arg, MonadIO m) => FullDomain -> arg -> CliConf -> m ()
runNotif fullDomain arg conf@(CliConf {..}) =
  cProtocol & \case
    Http -> runWithoutId @_ @_ @() httpRun
      fullDomain arg conf
    Https -> runWithoutId @_ @_ @() httpsRun
      fullDomain arg conf
    Websocket -> undefined

class (ToJSON arg, FromJSON res) => Protocol (a :: CliProtocol) arg res where
  run :: MonadIO m => Proxy a ->
        FullDomain -> Integer -> arg -> CliConf -> m res
  runWithoutId :: MonadIO m => Proxy a ->
        FullDomain -> arg -> CliConf -> m ()

instance (ToJSON arg, FromJSON b) => Protocol ('Http) arg b where
  run _ fullDomain rId arg CliConf {..} =
    liftIO $ runReq defaultHttpConfig handl
    where
      response :: FromJSON res => R.Req (JsonResponse res)
      response = buildHttpReqTemplate cPort
        (T.pack cHost)
        (reqObject fullDomain arg rId)
      handl :: R.Req b
      handl = do
        j <- response
        parseRemoteResult (responseBody j :: Res) &
          \case
            Left eo -> throwM eo
            Right va -> fromJSON va &
              \case
                Error s -> throwM (CliSysErr s)
                Success result -> pure result

  runWithoutId _ fullDomain arg (CliConf {..}) =
    liftIO $ runReq defaultHttpConfig handl
      where
        response :: FromJSON res => R.Req (JsonResponse res)
        response = buildHttpReqTemplate cPort
          (T.pack cHost)
          (reqObjectWithoutId fullDomain arg)
        handl :: R.Req ()
        handl = do
          j <- response
          parseRemoteResult (responseBody j :: Res) &
            \case
              Left eo -> throwM eo
              Right _ -> pure ()

instance (ToJSON arg, FromJSON b) => Protocol ('Https) arg b where
  run _ fullDomain rId arg CliConf {..} =
    liftIO $ runReq defaultHttpConfig handl
    where
      response :: FromJSON res => R.Req (JsonResponse res)
      response = buildHttpsReqTemplate cPort
        (T.pack cHost)
        (reqObject fullDomain arg rId)
      handl :: R.Req b
      handl = do
        j <- response
        parseRemoteResult (responseBody j :: Res) &
          \case
            Left eo -> throwM eo
            Right va -> fromJSON va &
              \case
                Error s -> throwM (CliSysErr s)
                Success result -> pure result

  runWithoutId _ fullDomain arg (CliConf {..}) =
    liftIO $ runReq defaultHttpConfig handl
      where
        response :: FromJSON res => R.Req (JsonResponse res)
        response = buildHttpsReqTemplate cPort
          (T.pack cHost)
          (reqObjectWithoutId fullDomain arg)
        handl :: R.Req ()
        handl = do
          j <- response
          parseRemoteResult (responseBody j :: Res) &
            \case
              Left eo -> throwM eo
              Right _ -> pure ()

reqObjectWithoutId :: ToJSON prm => String -> prm -> Value
reqObjectWithoutId mtd prm = object
  [ "jsonrpc" .= ("2.0" :: String),
    "method" .= mtd,
    "params" .= prm
  ]

reqObject :: ToJSON prm => String -> prm -> Integer -> Value
reqObject mtd prm reqId = object
  [ "jsonrpc" .= ("2.0" :: String),
    "method" .= mtd,
    "params" .= prm,
    "id" .= reqId
  ]

buildHttpReqTemplate :: FromJSON res => Int ->
  T.Text -> Value -> R.Req (JsonResponse res)
buildHttpReqTemplate p h body = req
  POST
  (http h)
  (ReqBodyJson body)
  jsonResponse
  (port p)

buildHttpsReqTemplate :: FromJSON res => Int ->
  T.Text -> Value -> R.Req (JsonResponse res)
buildHttpsReqTemplate p h body = req
  POST
  (https h)
  (ReqBodyJson body)
  jsonResponse
  (port p)
