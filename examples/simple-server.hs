{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (FromJSON, Object, Result (Error, Success), Value (Array), fromJSON, parseJSON, (.:))
import Data.Aeson.Types (Parser, Value (Object), parseMaybe, (.:))
import qualified Data.Vector as DV
import Network.RHC.Internal.Server
  ( BuildResponse (..),
    Method,
    MethodResult,
    RequestParse (..),
    runWarpServer,
  )
import Network.Wai.Handler.Warp (Port)

main :: IO ()
main = runWarpServer @PossibleRequests @PossibleResponses 3000

data PossibleRequests = AddReq [Integer] | CHReq ChangeRulerNameReq

data PossibleResponses = AddRes Integer | CHRes Ruler

instance RequestParse PossibleRequests where
  paramsParse "example.add" =
    Just
      (\v -> AddReq <$> parseJSON @[Integer] v)
  paramsParse "example.changeRulerName" =
    Just
      (\v -> CHReq <$> parseJSON @ChangeRulerNameReq v)

instance BuildResponse PossibleRequests PossibleResponses where
  performMethod (AddReq nums) = return (Right $ AddRes $ sum nums)
  performMethod (CHReq (ChangeRulerNameReq ident newName _)) =
    return (Right $ CHRes (Ruler ident newName))

-- instance for parcing (aeson)
instance FromJSON ChangeRulerNameReq where
  parseJSON (Object obj) = do
    changingPersonId <- obj .: "changingPersonId"
    newName <- obj .: "newName"
    oldName <- obj .: "oldName"
    return $ ChangeRulerNameReq changingPersonId newName oldName
  parseJSON _ = mempty

data ChangeRulerNameReq = ChangeRulerNameReq
  { changingPersonId :: Integer,
    newName :: String,
    oldName :: String
  }

data Ruler = Ruler
  { personId :: Integer,
    name :: String
  }