{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.RHC.Server where

type MethodName = String

data Key
data Value

newtype MethodArguments = MethodArguments [(Key, Value)]

type MethodResult = IO (Either String String)

newtype Method = Method {
        runMethod :: MethodArguments -> MethodResult
        }

type Methods = [(MethodName, Method)]

class RHCMethod a where
        initMethod :: a -> MethodArguments -> MethodResult


{-
todo: replace Value with FlexibleInstances with
        new constraint (class RHCTypes) that will type-cast aeson types to Value type
-}
instance RHCMethod b => RHCMethod (Value -> b) where
        initMethod f (MethodArguments (x:xs)) =
                initMethod (f (snd x)) (MethodArguments xs)
        initMethod _ _ = return $ Left "too few arguments"

instance RHCMethod (IO a) where
        initMethod f (MethodArguments []) = do
                                        _ <- f
                                        return $ Right "good"
        initMethod _ _ = return $ Left "too many arguments"

lookupMethod :: MethodName -> Methods -> Maybe Method
lookupMethod = lookup

runRHCServer :: Methods -> IO ()
runRHCServer methods = undefined