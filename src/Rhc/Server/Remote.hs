{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Rhc.Server.Remote
  ( RemoteEnv(..)
  , RPC(..)
  , ActionResponse
  , RemoteTable
  , RemoteAction
  , RemoteActionName
  , RemoteActionDomain
  , injectMethods
  ) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Aeson (Value, toJSON)
import Data.ByteString.Lazy (ByteString)
import Language.Haskell.TH
    (listE, Q, ExpQ, Type, Dec, Name, reify, runIO, Info(VarI), dyn)
import Language.Haskell.TH.Syntax
    (Q, Type, Dec, Name, reify, runIO, Info(VarI))
import Network.Wai.Handler.Warp (InvalidRequest, Port, run)

import Rhc.Internal

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

injectMethods :: [(Text, Name)] -> Q [Dec]
injectMethods names =
  [d|
    serv :: Port -> IO ()
    serv port = runWarp port
      (
        (
          $(packSenderDomains names (generateTable names))
        ) :
          $(generateTable names)
      )
  |]

generateTable :: [(Text, Name)] -> ExpQ
generateTable names = listE $ fmap attachAction names
  where
    attachAction :: (Text, Name) -> ExpQ
    attachAction (s, n) = [|(s, $(dyn "executeDecoded") $ $(dyn . show $ n))|]

packSenderDomains :: [(Text, Name)] -> ExpQ -> ExpQ
packSenderDomains ns xs = [| $(tuple) |]
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
