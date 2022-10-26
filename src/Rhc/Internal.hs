{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Rhc.Internal where

import Data.Aeson (ToJSON, FromJSON)
import Language.Haskell.TH
    ( Type, Name, NameSpace, Specificity, TyLit, TyVarBndr )
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

deriving instance ToJSON Name
deriving instance ToJSON Specificity
deriving instance ToJSON (TyVarBndr Specificity)
deriving instance ToJSON (TyVarBndr ())
deriving instance ToJSON TyLit
deriving instance ToJSON OccName
deriving instance ToJSON PkgName
deriving instance ToJSON NameSpace
deriving instance ToJSON ModName
deriving instance ToJSON NameFlavour
deriving instance ToJSON Type

deriving instance FromJSON Name
deriving instance FromJSON Specificity
deriving instance FromJSON (TyVarBndr Specificity)
deriving instance FromJSON (TyVarBndr ())
deriving instance FromJSON TyLit
deriving instance FromJSON OccName
deriving instance FromJSON PkgName
deriving instance FromJSON NameSpace
deriving instance FromJSON ModName
deriving instance FromJSON NameFlavour
deriving instance FromJSON Type