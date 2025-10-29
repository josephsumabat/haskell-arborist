{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildGraph.ModuleTargetOverrides
  ( TargetOverride (..)
  , ModuleTargetOverrides (..)
  , emptyTargetOverrides
  , lookupTargetOverride
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.String (IsString)
import Data.Text (Text)
import Hir.Parse (parseModuleTextFromText)
import Hir.Types qualified as Hir

newtype TargetOverride = TargetOverride {unTargetOverride :: Text}
  deriving stock (Eq, Show)
  deriving newtype (IsString, FromJSON, ToJSON)

newtype ModuleTargetOverrides = ModuleTargetOverrides
  { unModuleTargetOverrides :: HashMap Hir.ModuleText TargetOverride
  }
  deriving stock (Eq, Show)

instance FromJSON ModuleTargetOverrides where
  parseJSON = withObject "ModuleTargetOverrides" $ \obj -> do
    assignments <- traverse parseEntry (KeyMap.toList obj)
    pure $ ModuleTargetOverrides (HM.fromList assignments)
   where
    parseEntry (moduleNameKey, targetValue) = do
      target <- parseJSON targetValue
      let moduleNameText = Key.toText moduleNameKey
          moduleName = parseModuleTextFromText moduleNameText
      pure (moduleName, target)

instance ToJSON ModuleTargetOverrides where
  toJSON (ModuleTargetOverrides assignments) =
    Aeson.Object $ KeyMap.fromList (fmap toPair (HM.toList assignments))
   where
    toPair (moduleName, TargetOverride target) =
      let Hir.ModuleText {text = moduleText} = moduleName
       in (Key.fromText moduleText, Aeson.String target)

emptyTargetOverrides :: ModuleTargetOverrides
emptyTargetOverrides = ModuleTargetOverrides HM.empty

lookupTargetOverride :: Hir.ModuleText -> ModuleTargetOverrides -> Maybe TargetOverride
lookupTargetOverride moduleName (ModuleTargetOverrides assignments) =
  HM.lookup moduleName assignments
