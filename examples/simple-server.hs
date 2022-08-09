{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe, (.:))
import Network.RHC.Internal.Server
  ( RequestParamsParser,
    requestParamsParse,
    runWarpServer,
  )
import Network.Wai.Handler.Warp (Port)

main :: IO ()
main = runWarpServer @Ruler 3000

data Ruler = Ruler
  { name :: String,
    title :: String,
    age :: Integer
  }
  deriving (Show)

instance RequestParamsParser Ruler where
  requestParamsParse json = flip parseMaybe json $
    \obj -> do
      name <- obj .: "name"
      title <- obj .: "title"
      age <- obj .: "age"
      return (Ruler name title age)