{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.RHC.Internal.RPCErrors where
import Data.Aeson
import Control.Exception (Exception)

type ReqId = Integer

data ErrorCause
  = ParseError
  | InvalidRequest
  | MethodNotFound ReqId
  | InvalidParams
  | InternalError
  | ServerError Int deriving Exception

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

instance Show ErrorCause where
  show ParseError = "Invalid JSON was received by the server. \
    \An error occurred on the server while parsing the JSON text."
  show InvalidRequest = "The JSON sent is not a valid Request object."
  show (MethodNotFound _) = "The method does not exist / is not available."
  show InvalidParams = "Invalid method parameter(s)."
  show InternalError = "Internal JSON-RPC error."
  show (ServerError a) = show a

instance ToJSON ErrorCause where
  toJSON ParseError = toJSON @Integer (-32700)
  toJSON InvalidRequest = toJSON @Integer (-32600)
  toJSON (MethodNotFound _) = toJSON @Integer (-32601)
  toJSON InvalidParams = toJSON @Integer (-32602)
  toJSON InternalError = toJSON @Integer (-32603)
  toJSON (ServerError num) = toJSON num

{- instance Enum ErrorCause where
  fromEnum ParseError = -32700
  fromEnum InvalidRequest = -32600
  fromEnum MethodNotFound = -32601
  fromEnum InvalidParams = -32602
  fromEnum InternalError = -32603 -}

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
