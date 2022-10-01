{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.RHC.Internal.RPCErrors where
import Data.Aeson
import Control.Exception (Exception)

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
