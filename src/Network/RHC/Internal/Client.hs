{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.RHC.Internal.Client where

import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, object, (.=))
import Data.Aeson.Types (Result (..))
import qualified Data.Text as T
import Language.Haskell.TH (ExpQ, clause, funD, normalB, varP)
import Language.Haskell.TH.Syntax
import Network.HTTP.Req hiding (Http, Https)
import qualified Network.HTTP.Req as R
import Network.RHC.Internal.Orphans
import Network.RHC.Internal.RPCCommon (DomainMethods (..), MethodInfo (..))
import Network.RHC.Internal.RPCErrors (ErrorObject)
import Network.RHC.Internal.Server
import Control.Monad.Reader (ask)

data CliProtocol = Https | Http | Websocket

data CliConf = CliConf
  { cPort :: Int,
    cHost :: String,
    cProtocol :: CliProtocol
  }

newtype RemoteCall a = RemoteCall {runCall :: ReaderT CliConf IO a}
  deriving newtype
    ( Monad,
      Applicative,
      Functor,
      MonadReader CliConf,
      MonadIO,
      MonadThrow,
      MonadCatch
    )

load :: CliConf -> [String] -> Q [Dec]
load conf ds = runIO (queryDomains conf ds) >>= traverseDms

queryDomains :: CliConf -> [String] -> IO [DomainMethods]
queryDomains conf domains =
  runReq defaultHttpConfig $
    do
      let payload =
            object
              [ "jsonrpc" .= ("2.0" :: String),
                "method" .= ("sendDomains" :: String),
                "params" .= domains,
                "id" .= (-100 :: Integer)
              ]
      r <- requester "sendDomains" (-100) domains conf 
      case parseResponse (responseBody r :: Res) of
        Left eo -> throwM eo
        Right dms -> pure dms

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
    (CliConf p h Websocket) -> undefined
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

parseResponse :: FromJSON a => Res -> Either ErrorObject a
parseResponse = \case
  ResSuccess s va n -> case fromJSON va of
    Error str -> undefined -- delete in future
    Success any -> Right any
  ResErrsWithoutId s eo -> Left eo
  ResErrsWithId s eo n -> Left eo
  ResSystemErrs s eo -> Left eo
  ResVoid x0 -> Left undefined -- delete in future

rebuildSig :: Type -> Either String Type
rebuildSig = rebuild
  where
    rebuild (ForallT vrs ctx t) = fmap (ForallT vrs ctx) (rebuild t)
    rebuild (AppT (AppT _ farg) sarg) = Right (AppT (AppT ArrowT farg) (AppT monadTy sarg))
    rebuild _ = Left "not implemented"

    monadTy = ConT ''RemoteCall

traverseDms :: [DomainMethods] -> Q [Dec]
traverseDms ds = fmap (mconcat . mconcat) f
  where
    unpack dm x = declareServMethods (T.unpack dm) (T.unpack $ methodName x) (methodType x)
    f = mapM (\x -> mapM (unpack (domain x)) . methods $ x) ds

declareServMethods :: String -> String -> Value -> Q [Dec]
declareServMethods dm nm tyValue = do
  fnImpl <- funD fnName [clause [] (normalB fnBody) []]
  pure [typeSig, fnImpl]
  where
    fnName = mkName nm
    fnBody = [|
      \x -> do conf@(CliConf p h pr) <- ask
               liftIO $ runReq defaultHttpConfig
                        $ do 
                           j <- requester (dm ++ "." ++ nm) 42 x conf
                           case parseResponse (responseBody j) of
                             Left eo -> throwM eo
                             Right r -> pure r
      |]
    typeSig = SigD fnName tyD
    tyD :: Type
    tyD = case fromJSON tyValue of
      Error s -> undefined -- add error handling
      Success any -> case rebuildSig any of
        Left s -> undefined
        Right ty -> ty
