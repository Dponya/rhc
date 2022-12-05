{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rhc.Client
  ( CliConf (..)
  , CliProtocol (..)
  , RemoteCall (..)
  , load
  , remoteRunner
  , remoteNotifier
  ) where


import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, object, (.=))
import Data.Aeson.Types (Result (..))
import Data.Function
import Data.Proxy
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

newtype RemoteCall a = RemoteCall
  { unRemoteCall :: ReaderT CliConf IO a }
  deriving newtype
    ( Monad
    , Applicative
    , Functor
    , MonadReader CliConf
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

data ClientErrs = CliSysErr String
  deriving stock Show
  deriving anyclass Exception

remoteRunner :: CliConf -> RemoteCall a -> IO a
remoteRunner conf fn = runReaderT (unRemoteCall fn) conf

remoteNotifier :: CliConf -> RemoteCall a -> IO ()
remoteNotifier conf fn = runReaderT (unRemoteCall fn) conf >> pure ()

load :: CliConf -> [String] -> Q [Dec]
load conf ds = runIO (queryDomains conf ds) >>=
  \xs -> runIO (mapM rebuildDms xs) >>= traverseDms

queryDomains :: CliConf -> [String] -> IO [DomainMethods Type]
queryDomains conf@(CliConf {..}) domains =
  cProtocol &
    \case
      Http -> run httpRun "sendDomains" (-100) domains conf
      Https -> run httpsRun "sendDomains" (-100) domains conf
      Websocket -> undefined

rebuildSig :: Type -> Either String Type
rebuildSig = rebuild
  where
    rebuild (ForallT vrs ctx t) = fmap (ForallT vrs ctx) (rebuild t)
    rebuild (AppT (AppT _ farg) sarg)
      = Right (AppT (AppT ArrowT farg) (AppT monadTy sarg))
    rebuild _ = Left "not implemented"

    monadTy = ConT ''RemoteCall

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
      do conf@CliConf{..} <- ask
         cProtocol &
          \case
            Http -> run httpRun
              (domainName <> "." <> fnName) 42 arg conf
            Https -> run httpsRun
              (domainName <> "." <> fnName) 42 arg conf
            Websocket -> undefined
  |]

parseRemoteResult :: Res -> Either ErrorObject Value
parseRemoteResult = \case
  ResSuccess _ va _ -> Right va
  ResErrsWithoutId _ eo -> Left eo
  ResErrsWithId _ eo _ -> Left eo
  ResSystemErrs _ eo -> Left eo
  _ -> undefined

type FullDomain = String

httpRun :: Proxy ('Http)
httpRun = Proxy

httpsRun :: Proxy ('Https)
httpsRun = Proxy

websocketRun :: Proxy ('Websocket)
websocketRun = Proxy

class (ToJSON arg, FromJSON b) =>
  Protocol (a :: CliProtocol) arg b where
    run :: MonadIO m => Proxy a ->
          FullDomain -> Integer -> arg -> CliConf -> m b

instance (ToJSON arg, FromJSON b) => Protocol ('Http) arg b where
  run _ fullDomain rId arg conf =
    liftIO $ runReq defaultHttpConfig handl
    where
      handl :: (R.Req b)
      handl = do
        j <- requester fullDomain 42 arg conf
        parseRemoteResult (responseBody j :: Res) &
          \case
            Left eo -> throwM eo
            Right va -> fromJSON va &
              \case
                Error s -> throwM (CliSysErr s)
                Success result -> pure result

instance (ToJSON arg, FromJSON b) => Protocol ('Https) arg b where
  run _ fullDomain rId arg conf =
    liftIO $ runReq defaultHttpConfig handl
    where
      handl :: (R.Req b)
      handl = do
        j <- requester fullDomain 42 arg conf
        parseRemoteResult (responseBody j :: Res) &
          \case
            Left eo -> throwM eo
            Right va -> fromJSON va &
              \case
                Error s -> throwM (CliSysErr s)
                Success result -> pure result

requester ::
  (FromJSON res, ToJSON prm) =>
  String ->
  Integer ->
  prm ->
  CliConf ->
  R.Req (JsonResponse res)
requester mtd reqId prm =
  \case
    (CliConf p h Http) -> templateHttp p (T.pack h)
    (CliConf p h Https) -> templateHttps p (T.pack h)
    (CliConf _ _ Websocket) -> undefined -- not implemented yet
  where
    body =
      object
        [ "jsonrpc" .= ("2.0" :: String),
          "method" .= mtd,
          "params" .= prm,
          "id" .= reqId
        ]
    templateHttps p h =
      req
        POST
        (https h)
        (ReqBodyJson body)
        jsonResponse
        (port p)
    templateHttp p h =
      req
        POST
        (http h)
        (ReqBodyJson body)
        jsonResponse
        (port p)
