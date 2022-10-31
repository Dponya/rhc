{-# LANGUAGE TemplateHaskell #-}

module Rhc.Server.Remote
  ( RemoteEnv(..)
  , RPC(..)
  , ActionResponse
  , RemoteTable
  , RemoteAction
  , RemoteActionName
  , RemoteActionDomain
  , Namespace
  , method
  , domain
  , generate
  ) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Language.Haskell.TH
    (listE, Q, ExpQ, Dec, Name, reify, Info(VarI), dyn)
import Network.Wai.Handler.Warp (Port)

import Rhc.Internal ()

newtype RemoteEnv = RemoteEnv {
    table :: RemoteTable
  }

newtype RPC a = RPC { runRPC :: ReaderT RemoteEnv IO a }
  deriving newtype (
      Monad, Applicative, Functor,
      MonadReader RemoteEnv, MonadIO,
      MonadThrow, MonadCatch
    )

type ActionResponse = Value

type RemoteAction a b = a -> RPC b

type RemoteActionName = Text

type RemoteActionDomain = Text

type RemoteTable = [(
    RemoteActionName,
    RemoteAction ByteString ActionResponse
  )]

data Method = Method Text Name
  deriving stock Show

data Domain = Domain Text [Method]
  deriving stock Show

newtype Namespace = Namespace Domain
  deriving stock Show

domain :: Text -> [Method] -> [Namespace]
domain name mtds = [Namespace $ Domain name mtds]

method :: Text -> Name -> [Method]
method name identifier = [Method name identifier]

generate :: [Namespace] -> Q [Dec]
generate ns = injectMethods $ mconcat $ fmap construct ns
  where
    construct :: Namespace -> [(Text, Name)]
    construct (Namespace (Domain name mds)) = fmap (patterner name) mds
    patterner :: Text -> Method -> (Text, Name)
    patterner dname (Method name md) = (dname <> "." <> name, md)

injectMethods :: [(Text, Name)] -> Q [Dec]
injectMethods names =
  [d|
    remoteTable :: RemoteTable
    remoteTable = (
        (
          $(packSenderDomains names (generateTable names))
        ) :
          $(generateTable names)
      )

    serv :: Port -> IO ()
    serv port = runWarp port remoteTable
  |]

generateTable :: [(Text, Name)] -> ExpQ
generateTable names = listE $ fmap attachAction names
  where
    attachAction :: (Text, Name) -> ExpQ
    attachAction (s, n) = [|(s, $(dyn "executeDecoded") $ $(dyn . show $ n))|]

packSenderDomains :: [(Text, Name)] -> ExpQ -> ExpQ
packSenderDomains ns _ = [| $(tuple) |]
  where
    tuple = [| ("sendDomains", $(dyn "executeDecoded") $ $(dynamicApplied))|]
    arg :: ExpQ
    arg = let
        a = fmap (methodToExp . packMethodType) ns in listE a
    dynamicApplied = [| $(dyn "sendDomains") $(arg)|]

methodToExp :: Q (Text, Value) -> ExpQ
methodToExp m = m >>= (\x -> [|x|])

packMethodType :: (Text, Name) -> Q (Text, Value)
packMethodType (x, n) = do
  (VarI _ t _) <- reify n
  pure (x, toJSON t)
