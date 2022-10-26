{-# LANGUAGE OverloadedStrings #-}

module Rhc.Server.Response
  ( Res(..)
  , buildResponse
  , buildFromUser
  ) where

import Data.Aeson
  ( Object
  , FromJSON(..)
  , ToJSON(..)
  , eitherDecode
  , Result(..)
  , decode
  , encode
  , fromJSON
  , (.:)
  , (.=)
  , object
  )
import Data.Aeson.Types (Value(..), parseEither)
import Rhc.Server.Request (Req(..))
import Rhc.Server.Remote (ActionResponse)
import Rhc.Server.Error (ErrorObject)

import qualified Data.Aeson.KeyMap as KM


data Res
  = ResSuccess
      { resVersion :: String,
        result :: Value,
        resId :: Integer
      }
  | ResErrsWithoutId
      { resVersion :: String,
        resError :: ErrorObject
      }
  | ResErrsWithId
      { resVersion :: String,
        resError :: ErrorObject,
        resId :: Integer
      }
  | ResSystemErrs
      { resVersion :: String,
        resError :: ErrorObject
      }
  | ResVoid
      { void :: ()
      }
  deriving Show

instance FromJSON Res where
  parseJSON (Object v) = case KM.member "error" v of
    False -> case KM.member "id" v of
      False -> pure . ResVoid $ ()
      True -> ResSuccess
                  <$> v .: "jsonrpc"
                  <*> v .: "result"
                  <*> v .: "id"
    True -> case KM.member "id" v of
      False -> ResErrsWithoutId
                  <$> v .: "jsonrpc"
                  <*> v .: "error"
      True -> ResErrsWithId
                  <$> v .: "jsonrpc"
                  <*> v .: "error"
                  <*> v .: "id"
  parseJSON _ = mempty



instance ToJSON Res where
  toJSON (ResSuccess ver result rId) =
    object
      [ "jsonrpc" .= ver,
        "result" .= result,
        "id" .= rId
      ]
  toJSON (ResErrsWithoutId ver err) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err,
        "id" .= Null
      ]
  toJSON (ResErrsWithId ver err rId) =
    object
      [
        "jsonrpc" .= ver,
        "error" .= err,
        "id" .= rId
      ]
  toJSON (ResSystemErrs ver err) =
    object
      [ "jsonrpc" .= ver,
        "error" .= err
      ]
  toJSON (ResVoid ()) = Null

buildResponse :: Req -> ActionResponse -> Res
buildResponse Notif {} res = ResVoid ()
buildResponse (Req _ _ _ rId) result = ResSuccess "2.0" result rId

buildFromUser :: ErrorObject -> Res
buildFromUser obj = ResErrsWithoutId "2.0" obj
