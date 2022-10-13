{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.RHC.Internal.Utils where

import Data.Aeson (eitherDecode, FromJSON, Value, ToJSON(toJSON))
import Data.List (group, sortBy)
import Control.Monad.Catch ( MonadThrow(throwM) )
import Control.Monad.State (MonadState (get))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Syntax (Type)

import Network.RHC.Internal.Inspector
import Network.RHC.Internal.RPCCommon
    ( DomainMethods(DomainMethods),
      RemoteActionDomain,
      RemoteActionName,
      RemoteAction,
      ActionResponse, MethodInfo (MethodInfo) )
import Network.RHC.Internal.RPCErrors
    ( ErrorExecutionCause(InvalidParams) )

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as DF


executeDecoded ::
  forall a b.
  (FromJSON a, ToJSON b) =>
  RemoteAction a b ->
  RemoteAction BSL.ByteString ActionResponse
executeDecoded fun args =
  case eitherDecode args of
    Left s -> throwM InvalidParams
    Right prm -> fun prm >>= pure . toJSON

sendDomains :: [(T.Text, Value)] -> RemoteAction [String] [DomainMethods]
sendDomains table names = pure $ domains `includes` names
  where
    includes :: [DomainMethods] -> [String] -> [DomainMethods]
    includes ds (n:ns) = filter (isSame n) ds <> includes ds ns
    includes ds [] = []
    isSame n (DomainMethods dn _) = dn == T.pack n 
    domains = buildDomains table

buildDomains :: [(T.Text, Value)] -> [DomainMethods]
buildDomains table = fmap construct $ group . toSorted . fmap pack $ table
  where
    construct (DomainMethods n x : xs) =
      foldl append (DomainMethods n x) xs
    construct [] = undefined
    append (DomainMethods d fn) (DomainMethods _ fn1) =
      DomainMethods d (mappend fn fn1)
    toSorted = sortBy isEqual
    isEqual
      (DomainMethods x _)
      (DomainMethods x1 _) = compare x x1

    pack :: (T.Text, Value) -> DomainMethods
    pack (x1, x2) = DomainMethods (fst (divide x1)) [MethodInfo (snd (divide x1)) x2]

divide :: T.Text -> (RemoteActionDomain, RemoteActionName)
divide f = (domain, name)
  where
    [domain, name] = splitter
    splitter = T.splitOn "." f
