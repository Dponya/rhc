{-# LANGUAGE DeriveFunctor #-}

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
  } deriving stock (Show, Generic, Functor)
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

    isSame :: String -> DomainMethods ty -> Bool
    isSame n (DomainMethods dn _) = dn == T.pack n

    domains = buildDomains table

buildDomains :: [(T.Text, Value)] -> [DomainMethods Value]
buildDomains table =
  fmap construct $
       group .
       toSorted . 
       fmap pack $ table
  where
    construct :: [DomainMethods ty] -> DomainMethods ty
    construct (DomainMethods n x : xs) =
      foldl append (DomainMethods n x) xs
    construct [] = DomainMethods mempty mempty

    append :: DomainMethods ty -> DomainMethods ty -> DomainMethods ty
    append (DomainMethods d fn) (DomainMethods _ fn1) =
      DomainMethods d (mappend fn fn1)

    toSorted :: [DomainMethods ty] -> [DomainMethods ty] 
    toSorted = sortBy isEqual

    isEqual :: DomainMethods ty -> DomainMethods ty1 -> Ordering
    isEqual
      (DomainMethods x _)
      (DomainMethods x1 _) = compare x x1

    pack :: (T.Text, Value) -> DomainMethods Value
    pack (path, fn) = DomainMethods
      (fst . divideFnPath $ path)
      [MethodInfo (snd . divideFnPath $ path) fn]

divideFnPath :: T.Text -> (RemoteActionDomain, RemoteActionName)
divideFnPath fnName = (head divided, head $ tail divided)
  where
    divided = T.splitOn "." fnName
