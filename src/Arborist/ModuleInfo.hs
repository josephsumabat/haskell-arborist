{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Arborist.ModuleInfo
  ( DirName (..)
  , ModuleLocation (..)
  , ModuleInfo (..)
  , ModuleInfoCache (..)
  , moduleInfoCacheFromMap
  , moduleInfoCacheToMap
  , readModuleInfoCache
  , writeModuleInfoCache
  , moduleTextToText
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, encode, object, withObject, (.:), (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hir.Parse (parseModuleTextFromText)
import Hir.Types qualified as Hir

newtype DirName = DirName {dirText :: Text}
  deriving (Eq, Ord, Show, Generic, Hashable)

data ModuleLocation
  = LocalModule !DirName
  | ExternalModule
  deriving (Eq, Show, Generic)

data ModuleInfo = ModuleInfo
  { location :: !ModuleLocation
  , imports :: !(Set Hir.ModuleText)
  }
  deriving (Eq, Show, Generic)

newtype ModuleInfoCache = ModuleInfoCache {unModuleInfoCache :: HashMap Hir.ModuleText ModuleInfo}
  deriving (Eq, Show, Generic)

moduleInfoCacheFromMap :: HashMap Hir.ModuleText ModuleInfo -> ModuleInfoCache
moduleInfoCacheFromMap = ModuleInfoCache

moduleInfoCacheToMap :: ModuleInfoCache -> HashMap Hir.ModuleText ModuleInfo
moduleInfoCacheToMap (ModuleInfoCache cache) = cache

readModuleInfoCache :: FilePath -> IO ModuleInfoCache
readModuleInfoCache path = do
  bytes <- BL.readFile path
  either fail pure (eitherDecode bytes)

writeModuleInfoCache :: FilePath -> ModuleInfoCache -> IO ()
writeModuleInfoCache path cache =
  BL.writeFile path (encode cache)

instance ToJSON DirName where
  toJSON (DirName text) = toJSON text

instance FromJSON DirName where
  parseJSON v = DirName <$> parseJSON v

instance ToJSON ModuleLocation where
  toJSON location =
    case location of
      LocalModule dir ->
        object
          [ "type" .= T.pack "local"
          , "directory" .= dir
          ]
      ExternalModule -> object ["type" .= T.pack "external"]

instance FromJSON ModuleLocation where
  parseJSON = withObject "ModuleLocation" $ \obj -> do
    tag <- obj .: "type"
    case (tag :: Text) of
      "local" -> LocalModule <$> obj .: "directory"
      "external" -> pure ExternalModule
      other -> fail ("Unknown module location tag: " <> T.unpack other)

instance ToJSON ModuleInfo where
  toJSON ModuleInfo {location, imports} =
    object
      [ "location" .= location
      , "imports" .= map moduleTextToText (Set.toList imports)
      ]

instance FromJSON ModuleInfo where
  parseJSON = withObject "ModuleInfo" $ \obj -> do
    location <- obj .: "location"
    importNames <- obj .: "imports"
    imports <- traverse parseModuleName importNames
    pure ModuleInfo {location, imports = Set.fromList imports}
   where
    parseModuleName :: Text -> Parser Hir.ModuleText
    parseModuleName nameText = pure (parseModuleTextFromText nameText)

instance ToJSON ModuleInfoCache where
  toJSON (ModuleInfoCache cache) =
    object
      [ Key.fromText (moduleTextToText moduleName) .= info
      | (moduleName, info) <- HM.toList cache
      ]

instance FromJSON ModuleInfoCache where
  parseJSON = withObject "ModuleInfoCache" $ \obj -> do
    entries <- traverse parseEntry (KeyMap.toList obj)
    pure (ModuleInfoCache (HM.fromList entries))
   where
    parseEntry :: (Key.Key, Value) -> Parser (Hir.ModuleText, ModuleInfo)
    parseEntry (moduleKey, value) = do
      info <- parseJSON value
      let moduleName = parseModuleTextFromText (Key.toText moduleKey)
      pure (moduleName, info)

moduleTextToText :: Hir.ModuleText -> Text
moduleTextToText = (.text)
