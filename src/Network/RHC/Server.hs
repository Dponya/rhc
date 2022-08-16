{-# LANGUAGE FlexibleInstances #-}

module Network.RHC.Server where

import Data.Aeson (Object)

type MethodName = String

data Key

data Value
  = VString String
  | VInteger Integer
  | VBool Bool
  | VDouble Double
  | VArray [Value]
  | VObject [(String, Value)]
  | VNull

newtype MethodArguments = MethodArguments [Value]

margs = MethodArguments [VString "blah", VInteger 1]

--newtype MethodArguments = MethodArguments [(Key, Value)]

type MethodResult = IO (Either String String)

newtype Method = Method
  { runMethod :: MethodArguments -> MethodResult
  }

type Methods = [(MethodName, Method)]

method :: RHCMethod a => a -> Method
method f = Method (initMethod f)

class RHCMethod a where
  initMethod :: a -> MethodArguments -> MethodResult

{-
todo: replace Value with FlexibleInstances with
        new constraint (class RHCTypes) that will type-cast aeson types to Value type
-}

lookupMethod :: MethodName -> Methods -> Maybe Method
lookupMethod = lookup

runRHCServer :: Methods -> IO ()
runRHCServer methods = undefined