{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson ((.:), FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe, (.:), Value(Object), Parser)
import Network.RHC.Internal.Server
  (
    runWarpServer,
    RequestParse,
    paramsParse
  )
import qualified Data.Vector as DV
import Network.Wai.Handler.Warp (Port)
import Data.Aeson (Object)
import Data.Aeson (Value(Array))

main :: IO ()
main = runWarpServer @Rulers 3000

data Ruler = Ruler
  { name :: String,
    title :: String,
    age :: Integer
  }
  deriving (Show, RequestParse)

newtype Rulers = Rulers [Ruler] deriving (Show, RequestParse)

instance FromJSON Ruler where
  parseJSON (Object obj) = do
        name <- obj .: "name"
        title <- obj .: "title"
        age <- obj .: "age"
        return $ Ruler name title age
  parseJSON _ = mempty

instance FromJSON Rulers where
  parseJSON (Array v) = do
      rulersList <- mapM (parseJSON :: Value -> Parser Ruler) v
      return $ Rulers $ DV.toList rulersList
  parseJSON _ = mempty