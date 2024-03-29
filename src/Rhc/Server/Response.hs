module Rhc.Server.Response
  ( Res(..)
  , buildResponse
  , buildFromUser
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:)
  , (.=)
  , object
  )
import Data.Aeson.Types (Value(..))
import Rhc.Server.Request (Req(..), ReqWithId(..), Notif(..))
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
  deriving stock Show

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
  parseJSON Null = pure $ ResVoid ()
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

buildResponse :: Req Value -> Res
buildResponse (FullReq (ReqWithId _ _ result rId)) = ResSuccess "2.0" result rId
buildResponse (ReqNotif _) = ResVoid ()

buildFromUser :: ErrorObject -> Res
buildFromUser obj = ResErrsWithoutId "2.0" obj
