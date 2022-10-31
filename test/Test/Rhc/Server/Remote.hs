{-# LANGUAGE TemplateHaskell #-}

module Test.Rhc.Server.Remote where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Test.Hspec (Spec, describe, it, shouldBe)

import Rhc
  ( domain
  , method
  , generate
  , RemoteAction
  , executeDecoded
  , runWarp
  , sendDomains
  )

makeCoffee :: RemoteAction String [String]
makeCoffee x = pure ["some", "pre-defined", "words"]

doSome :: RemoteAction [Int] Int
doSome (x:x2:xs) = liftIO $ print x >> pure (x + x2)
doSome xs = liftIO $ print xs >> pure 0

generate $ domain "example" (method "doSome" 'doSome)
          <> domain "coffee" (method "makeCoffee" 'makeCoffee)

remoteSpec :: Spec
remoteSpec =
  describe "generate" $ do
    it "generates functions and defines correct remoteTable" $ do
      let utilityFn = head remoteTable
      let doSomeFn = head . tail $ remoteTable
      let makeCoffeeFn = head $ tail . tail $ remoteTable 

      fst doSomeFn `shouldBe` "example.doSome"
      fst makeCoffeeFn `shouldBe` "coffee.makeCoffee"
      fst utilityFn `shouldBe` "sendDomains"