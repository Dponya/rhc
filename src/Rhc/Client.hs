{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Rhc.Client
  ( CliConf (..)
  , CliProtocol (..)
  , RemoteCall (..)
  , load
  ) where


import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow (throwM))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, object, (.=))
import Data.Aeson.Types (Result (..))
import Language.Haskell.TH (clause, funD, normalB)
import Language.Haskell.TH.Syntax
    (mkName, Q, Type(ConT, ForallT, ArrowT, AppT), Dec(SigD), runIO)
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

newtype RemoteCall a = RemoteCall { runCall :: ReaderT CliConf IO a }
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

load :: CliConf -> [String] -> Q [Dec]
load conf ds = runIO (queryDomains conf ds) >>= traverseDms

queryDomains :: CliConf -> [String] -> IO [DomainMethods Type]
queryDomains conf domains =
  runReq defaultHttpConfig $
    do
      r <- requester "sendDomains" (-100) domains conf 
      case parseRemoteResult (responseBody r :: Res) of
        Left eo -> throwM eo
        Right va -> case fromJSON va of
          Error s -> throwM (CliSysErr s)
          Success result -> pure (result :: [DomainMethods Type])

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

parseRemoteResult :: Res -> Either ErrorObject Value
parseRemoteResult = \case
  ResSuccess _ va _ -> Right va
  ResErrsWithoutId _ eo -> Left eo
  ResErrsWithId _ eo _ -> Left eo
  ResSystemErrs _ eo -> Left eo
  _ -> undefined

rebuildSig :: Type -> Either String Type
rebuildSig = rebuild
  where
    rebuild (ForallT vrs ctx t) = fmap (ForallT vrs ctx) (rebuild t)
    rebuild (AppT (AppT _ farg) sarg) = Right (AppT (AppT ArrowT farg) (AppT monadTy sarg))
    rebuild _ = Left "not implemented"

    monadTy = ConT ''RemoteCall

traverseDms :: [DomainMethods Type] -> Q [Dec]
traverseDms ds = fmap (mconcat . mconcat) f
  where
    unpack dm x = declareServMethods (T.unpack dm) (T.unpack $ methodName x) (methodType x)
    f = mapM (\x -> mapM (unpack (domain x)) . methods $ x) ds

declareServMethods :: String -> String -> Type -> Q [Dec]
declareServMethods dm nm ty = do
  fnImpl <- funD fnName [clause [] (normalB fnBody) []]
  pure [typeSig, fnImpl]
  where
    fnName = mkName nm
    fnBody = [|
      \x -> do conf <- ask
               liftIO $
                runReq defaultHttpConfig
                  $ do
                     j <- requester (dm ++ "." ++ nm) 42 x conf
                     case parseRemoteResult (responseBody j :: Res) of
                      Left eo -> throwM eo
                      Right va -> case fromJSON va of
                        Error s -> throwM (CliSysErr s)
                        Success result -> pure result
      |]
    typeSig = SigD fnName tyD
    tyD = case rebuildSig ty of
      Left _ -> undefined
      Right ty' -> ty'
