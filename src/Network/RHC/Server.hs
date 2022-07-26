module Network.RHC.Server where

type MethodName = String
type Method = * -> IO ()
newtype Methods = Methods [(MethodName, Method)]

runRHCServer :: Methods -> IO ()
runRHCServer methods = undefined
