{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.RHC.Internal.Server
  (
    MethodPerform (..),
    RequestParse (..),
    runWarpServer,
  )
import Network.Wai.Handler.Warp (Port)

main :: IO ()
main = runWarpServer @PossibleRequests 3000

data PossibleRequests = AddReq [Integer] | CHReq ChangeRulerNameReq

-- data PossibleResponses = AddRes Integer | CHRes Ruler

instance RequestParse PossibleRequests where
  paramsParse "example.add" =
    Just (\v -> AddReq <$> parseJSON @[Integer] v)
  paramsParse "example.changeRulerName" =
    Just (\v -> CHReq <$> parseJSON @ChangeRulerNameReq v)
  paramsParse _ = Nothing

instance MethodPerform PossibleRequests where
  performMethod "example.add"
      (AddReq nums) = return . Right . toJSON . sum $ nums
  
  performMethod "example.changeRulerName"
      (CHReq (ChangeRulerNameReq ident newName _)) =
    (return . Right $ toJSON (Ruler ident newName))

-- instance for parcing (aeson)
instance FromJSON ChangeRulerNameReq where
  parseJSON (Object obj) = do
    changingPersonId <- obj .: "changingPersonId"
    newName <- obj .: "newName"
    oldName <- obj .: "oldName"
    return $ ChangeRulerNameReq changingPersonId newName oldName
  parseJSON _ = mempty

instance ToJSON Ruler where
  toJSON (Ruler personId name) =
    object [
      "personId" .= personId,
      "name" .= name
    ]

data ChangeRulerNameReq = ChangeRulerNameReq
  { changingPersonId :: Integer,
    newName :: String,
    oldName :: String
  }

data Ruler = Ruler
  { personId :: Integer,
    name :: String
  }
