{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Data.Aeson (Value, encode, ToJSON (toJSON))
import Control.Monad.Catch (MonadThrow(throwM))

import Rhc

maxLength :: RemoteAction [Integer] Integer
maxLength [] = pure 0
maxLength xs = pure $ maximum xs

concatWholeList :: RemoteAction [String] String
concatWholeList xs = pure $ concat xs

checkMaxBound :: RemoteAction Int Int
checkMaxBound x =
  if x == maxBound
    then pure minBound
    else throwM err
  where
    err = ErrorObject 
      (ErrorServCause (ServerError (-32000)))
      "Number isn't a maxBound"

injectMethods
  [ ("lists.maxLength", 'maxLength)
  , ("lists.concatWholeList", 'concatWholeList)
  , ("lists.checkMaxBound", 'checkMaxBound)
  ]

main :: IO ()
main = hspec $ do
  describe "mainThread" $ do
    it "is single request with id was well replied with computed result" $ do
      let params = toJSON @[Integer] [1,2,3,4]
      let req = encode (Req "2.0" "lists.maxLength" params 532)
      let expected = toJSON $ ResSuccess "2.0" (toJSON @Integer 4 :: Value) 532

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)

    it "is multiple requests with id was well replied all" $ do
      let prm1 = toJSON @[Integer] [42, 321, 909]
      let prm2 = toJSON @[String] ["I'm ", "the ", "king!"]
      let reqs = encode
            [ Req "2.0" "lists.maxLength" prm1 532
            , Req "2.0" "lists.concatWholeList" prm2 442
            ]
      let expected = toJSON
            [ toJSON $ ResSuccess "2.0" (toJSON @Integer 909 :: Value) 532
            , toJSON $ ResSuccess "2.0" (toJSON @String "I'm the king!" :: Value) 442
            ]

      sut <- mainThread reqs remoteTable

      sut `shouldBe` (expected :: Value)

    it "request with uncompleted structure is invalid" $ do
      let req = "{ \"jsonrpc\": \"2.0\", \"method\": \"lists.concatWholeList\" }"
      let errMsg = "The JSON sent is not a valid Request object."
      let expected = toJSON $
            ResErrsWithoutId "2.0"
              (ErrorObject (ErrorParseCause InvalidRequest) errMsg)

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)

    it "request with uncorrect json syntax is invalid" $ do
      let req = "{ \"jsonrpc\": \"2.0\", \"method\": \"lists.concatWholeList\" , }"
      let errMsg = "Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text."
      let expected = toJSON $
            ResErrsWithoutId "2.0"
              (ErrorObject (ErrorParseCause ParseError) errMsg)

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)

    it "request with method that doesn't exist is invalid" $ do
      let params = toJSON @[Integer] [1,2,3]
      let req = encode (Req "2.0" "lists.imNotExist" params 324)
      let errMsg = "The method does not exist / is not available."
      let expected = toJSON $
            ResErrsWithId "2.0"
              (ErrorObject (ErrorExecutionCause MethodNotFound) errMsg) 324

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)

    it "request with invalid params for method is invalid too" $ do
      let params = toJSON @String "im not right"
      let req = encode (Req "2.0" "lists.maxLength" params 324)
      let errMsg = "Invalid method parameter(s)."
      let expected = toJSON $
            ResErrsWithId "2.0"
              (ErrorObject (ErrorExecutionCause InvalidParams) errMsg) 324

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)

    it "method throws custom exception and replies client as well" $ do
      let params = toJSON @Int 52131
      let req = encode (Req "2.0" "lists.checkMaxBound" params 1000)
      let errMsg = "Number isn't a maxBound"
      let expected = toJSON $
            ResErrsWithId "2.0"
                (ErrorObject (ErrorServCause (ServerError (-32000))) errMsg) 1000

      sut <- mainThread req remoteTable

      sut `shouldBe` (expected :: Value)
