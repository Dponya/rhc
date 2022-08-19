{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Aeson ((.:), FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe, (.:), Value(Object), Parser)
import Network.RHC.Internal.Server
  (
    runWarpServer,
    RequestParse,
    paramsParse,
    initMethod,
    MethodResult
  )
import qualified Data.Vector as DV
import Network.Wai.Handler.Warp (Port)
import Data.Aeson (Object)
import Data.Aeson (Value(Array))

main :: IO ()
main = runWarpServer @Ruler 3000

data Ruler =
  Ruler {
    personId :: Integer,
    name :: String
  } deriving (Show, RequestParse)

data ChangeRulerNameReq =
  ChangeRulerNameReq {
    changingPersonId :: Integer,
    newName :: String,
    oldName :: String
  } deriving (Show, RequestParse)
-- RequestParse is necessary for defining your datatype by library

-- instance for parcing (aeson)
instance FromJSON ChangeRulerNameReq where
  parseJSON (Object obj) = do
        changingPersonId <- obj .: "changingPersonId"
        newName <- obj .: "newName"
        oldName <- obj .: "oldName"
        return $ ChangeRulerNameReq changingPersonId newName oldName
  parseJSON _ = mempty

instance FromJSON Ruler where
  parseJSON (Object obj) = do
        personId <- obj .: "personId"
        name <- obj .: "name"
        return $ Ruler personId name
  parseJSON _ = mempty

-- simple methods
changeName :: ChangeRulerNameReq -> IO Ruler
changeName (ChangeRulerNameReq {changingPersonId, newName, oldName})
            = return (Ruler changingPersonId newName)

add :: Int -> Int -> IO ()
add x y = print (x + y)

ex1 :: ChangeRulerNameReq -> MethodResult
ex1 = initMethod changeName

ex :: [Int] -> MethodResult
ex = initMethod add