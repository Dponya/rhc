{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Network.HTTP.Req as R
import Network.RHC.Internal.Client (queryDomains, load, CliConf (..), CliProtocol(..))

main :: IO ()
main = print ()

$(load (CliConf 3000 "localhost" Http) ["example", "coffee"])

{-

load ["example", "offers", "past"]

remoteRunner = runCall (1500 "localhosts" Websocket)

sendEmailTemplates :: [Integer] -> RemoteCall [Integer]
sendEmailTemplate nums =
    do result <- doSome example $ nums
       return $ map (+1) result

main = remoteRunner $ sendEmailTemplates [1,2,3]

-}
