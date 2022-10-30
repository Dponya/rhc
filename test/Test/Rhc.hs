module Test.Rhc (rhcSpec) where

import Test.Hspec (Spec, describe)

import Test.Rhc.Server (serverSpec)

rhcSpec :: Spec
rhcSpec = describe "Rhc" serverSpec
