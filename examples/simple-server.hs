{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE TypeApplications  #-}
module Main where

import Network.RHC.Internal.Server(
        runWarpServer,
        RequestParamsParser,
        requestParamsParse
        )
import Data.Aeson ( (.:) )
import Data.Aeson.Types ( (.:), parseMaybe )
import Network.Wai.Handler.Warp (Port)

main :: IO ()
main = runWarpServer @Ruler 3000

data Ruler = Ruler {
                name :: String,
                title :: String,
                age :: Integer
        } deriving Show

instance RequestParamsParser Ruler where
        requestParamsParse json = flip parseMaybe json $
                                \obj -> do
                                        name <- obj .: "name"
                                        title <- obj .: "title"
                                        age <- obj .: "age"
                                        return (Ruler name title age)