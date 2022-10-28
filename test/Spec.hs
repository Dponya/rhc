{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Data.Aeson (Value, encode, ToJSON (toJSON))

import Rhc

maxLength :: RemoteAction [Integer] Integer
maxLength [] = pure 0
maxLength xs = pure $ maximum xs

concatWholeList :: RemoteAction [String] String
concatWholeList xs = pure $ concat xs

injectMethods
  [ ("lists.maxLength", 'maxLength)
  , ("lists.concatWholeList", 'concatWholeList)
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
