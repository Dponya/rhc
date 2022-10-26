{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Rhc.Server.Domain
  ( MethodInfo(..)
  , DomainMethods(..)
  , sendDomains
  ) where

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.List (group, sortBy)
import GHC.Generics (Generic)

import Rhc.Server.Remote (RemoteAction, RemoteActionName, RemoteActionDomain)

import qualified Data.Text as T

data MethodInfo ty = MethodInfo
  { methodName :: T.Text
  , methodType :: ty
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DomainMethods ty = DomainMethods
  { domain :: T.Text
  , methods :: [MethodInfo ty]
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq (DomainMethods a) where
  (DomainMethods d _) == (DomainMethods d1 _) = d == d1

sendDomains :: [(T.Text, Value)] -> RemoteAction [String] [DomainMethods Value]
sendDomains table names = pure $ domains `includes` names
  where
    includes :: [DomainMethods Value] -> [String] -> [DomainMethods Value]
    includes ds (n:ns) = filter (isSame n) ds <> includes ds ns
    includes _ [] = []
    isSame n (DomainMethods dn _) = dn == T.pack n
    domains = buildDomains table

buildDomains :: [(T.Text, Value)] -> [DomainMethods Value]
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

    pack :: (T.Text, Value) -> DomainMethods Value
    pack (x1, x2) = DomainMethods (fst (divide x1)) [MethodInfo (snd (divide x1)) x2]

divide :: T.Text -> (RemoteActionDomain, RemoteActionName)
divide f = (domain, name)
  where
    [domain, name] = splitter
    splitter = T.splitOn "." f
