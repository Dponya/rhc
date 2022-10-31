module Test.Rhc (rhcSpec) where

import Test.Hspec (Spec, describe)

import Test.Rhc.Server (serverSpec)
import Test.Rhc.Server.Remote (remoteSpec)

rhcSpec :: Spec
rhcSpec = describe "Rhc" $ do
  serverSpec
  remoteSpec
