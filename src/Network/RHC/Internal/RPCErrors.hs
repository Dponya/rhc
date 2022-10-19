{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.RHC.Internal.RPCErrors where
import Data.Aeson
import Control.Exception (Exception)
import Data.Aeson.KeyMap
import Data.List (find)

type ReqId = Integer

data ErrorParseCause
  = ParseError
  | InvalidRequest deriving Exception

data ErrorExecutionCause
  = MethodNotFound
  | InvalidParams deriving Exception

data ErrorServCause
  = InternalError
  | ServerError Int deriving Exception

data ErrorCause
  = ErrorParseCause ErrorParseCause
  | ErrorExecutionCause ErrorExecutionCause
  | ErrorServCause ErrorServCause

data ErrorObject
  = ErrorObject
      { code :: ErrorCause,
        message :: String
      }
  | ErrorObjectAdditional
      { code :: ErrorCause,
        message :: String,
        additional :: Value
      } deriving (Show, Exception)

errObject :: ErrorCause -> ErrorObject
errObject cause = ErrorObject cause (show cause)

instance Show ErrorParseCause where
  show ParseError = "Invalid JSON was received by the server. \
    \An error occurred on the server while parsing the JSON text."
  show InvalidRequest = "The JSON sent is not a valid Request object."

instance Show ErrorExecutionCause where
  show MethodNotFound = "The method does not exist / is not available."
  show InvalidParams = "Invalid method parameter(s)."

instance Show ErrorServCause where
  show InternalError = "Internal JSON-RPC error."
  show (ServerError a) = show a

instance Show ErrorCause where
  show (ErrorParseCause a) = show a
  show (ErrorExecutionCause a) = show a
  show (ErrorServCause a) = show a

instance ToJSON ErrorParseCause where
  toJSON ParseError = toJSON @Integer (-32700)
  toJSON InvalidRequest = toJSON @Integer (-32600)

instance ToJSON ErrorExecutionCause where
  toJSON MethodNotFound = toJSON @Integer (-32601)
  toJSON InvalidParams = toJSON @Integer (-32602)

instance ToJSON ErrorServCause where
  toJSON InternalError = toJSON @Integer (-32603)
  toJSON (ServerError num) = toJSON num

instance ToJSON ErrorCause where
  toJSON (ErrorParseCause a) = toJSON a
  toJSON (ErrorExecutionCause a) = toJSON a
  toJSON (ErrorServCause a) = toJSON a

instance FromJSON ErrorCause where
  parseJSON a = case fromJSON @Int a of
    Error s -> mempty
    Success n -> pure result
      where
        result = case elem of
          Nothing -> ErrorServCause (ServerError n)
          Just (_, x) -> x
        elem = find (\(idx, _) -> idx == n) codes
        codes :: [(Int, ErrorCause)]
        codes = [
          (-32700, ErrorParseCause ParseError),
          (-32600, ErrorParseCause InvalidRequest),
          (-32601, ErrorExecutionCause MethodNotFound), 
          (-32602, ErrorExecutionCause InvalidParams),
          (-32603, ErrorServCause InternalError)
          ]

instance ToJSON ErrorObject where
  toJSON (ErrorObject code msg) =
    object
      [ "code" .= code,
        "message" .= msg
      ]
  toJSON
    (ErrorObjectAdditional code msg additional) =
      object
        [ "code" .= code,
          "message" .= msg,
          "data" .= additional
        ]

instance FromJSON ErrorObject where
  parseJSON (Object v) =
    case member "data" v of
      True -> ErrorObjectAdditional
                <$> v .: "code"
                <*> v .: "message"
                <*> v .: "data"
      False -> ErrorObject
                <$> v .: "code"
                <*> v .: "message"
  parseJSON _ = mempty