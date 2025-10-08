module Arborist.Config (
  ArboristConfig (..),
  loadArboristConfig,
  defaultConfigPath,
  allSourceRoots,
) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import GHC.Stack (withFrozenCallStack)
import System.Directory qualified as Dir

data ArboristConfig = ArboristConfig
  { mutableSourceRoots :: [FilePath]
  , immutableSourceRoots :: [FilePath]
  }
  deriving (Show, Eq)

defaultConfigPath :: FilePath
defaultConfigPath = "arborist-config.json"

instance FromJSON ArboristConfig where
  parseJSON = Aeson.withObject "ArboristConfig" $ \obj -> do
    mutableRoots <- obj .: "mutableSourceRoots"
    immutableRoots <- obj .: "immutableSourceRoots"
    pure $ ArboristConfig (sanitize mutableRoots) (sanitize immutableRoots)
   where
    sanitize :: [FilePath] -> [FilePath]
    sanitize = filter (not . null)

loadArboristConfig :: Maybe FilePath -> IO ArboristConfig
loadArboristConfig mPath = withFrozenCallStack $ do
  let configPath = fromMaybe defaultConfigPath mPath
  exists <- Dir.doesFileExist configPath
  unless exists $ do
    fail $ "Arborist configuration not found: " <> configPath
  raw <- BS.readFile configPath
  case Aeson.eitherDecodeStrict raw of
    Left err -> fail $ "Unable to parse Arborist configuration: " <> err
    Right cfg ->
      if null (allSourceRoots cfg)
        then fail "Arborist configuration must specify at least one visible source root"
        else pure cfg

allSourceRoots :: ArboristConfig -> [FilePath]
allSourceRoots ArboristConfig {mutableSourceRoots, immutableSourceRoots} =
  mutableSourceRoots ++ immutableSourceRoots
