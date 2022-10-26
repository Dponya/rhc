{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Rhc.Internal where

import Data.Aeson (ToJSON, FromJSON)
import Language.Haskell.TH.Syntax
    ( Type,
      Name,
      ModName,
      NameFlavour,
      NameSpace,
      OccName,
      PkgName,
      Specificity,
      TyLit,
      TyVarBndr )

deriving anyclass instance ToJSON Name
deriving anyclass instance ToJSON Specificity
deriving anyclass instance ToJSON (TyVarBndr Specificity)
deriving anyclass instance ToJSON (TyVarBndr ())
deriving anyclass instance ToJSON TyLit
deriving anyclass instance ToJSON OccName
deriving anyclass instance ToJSON PkgName
deriving anyclass instance ToJSON NameSpace
deriving anyclass instance ToJSON ModName
deriving anyclass instance ToJSON NameFlavour
deriving anyclass instance ToJSON Type

deriving anyclass instance FromJSON Name
deriving anyclass instance FromJSON Specificity
deriving anyclass instance FromJSON (TyVarBndr Specificity)
deriving anyclass instance FromJSON (TyVarBndr ())
deriving anyclass instance FromJSON TyLit
deriving anyclass instance FromJSON OccName
deriving anyclass instance FromJSON PkgName
deriving anyclass instance FromJSON NameSpace
deriving anyclass instance FromJSON ModName
deriving anyclass instance FromJSON NameFlavour
deriving anyclass instance FromJSON Type