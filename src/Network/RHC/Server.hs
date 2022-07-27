{-# LANGUAGE RankNTypes #-}
module Network.RHC.Server where

type MethodName = String
newtype Method = Method { runMethod :: forall a b. a -> b }
type Methods = [(MethodName, Method)]

lookupMethod :: MethodName -> Methods -> Maybe Method
lookupMethod = lookup

runRHCServer :: Methods -> IO ()
runRHCServer methods = undefined