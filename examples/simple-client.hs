module Main where

import Network.HTTP.Req
import Network.RHC.Internal.Client (queryDomains)

main :: IO ()
main = queryDomains ["example"] >> print ()

{-

load ["example", "offers", "past"]

remoteRunner = runRemote 1500 "localhosts"

sendEmailTemplates :: [Integer] -> RemoteCall [Integer]
sendEmailTemplate nums =
    do result <- doSome example $ nums
       return $ map (+1) result

main = remoteRunner $ sendEmailTemplates [1,2,3]

-}
