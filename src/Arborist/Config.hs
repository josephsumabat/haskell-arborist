{-# LANGUAGE TemplateHaskell #-}

module Arborist.Config (
  ArboristConfig (..),
  loadArboristConfig,
  defaultConfigPath,
  allSourceRoots,
) where

import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import GHC.Stack (withFrozenCallStack)
import System.Directory qualified as Dir

data ArboristConfig = ArboristConfig
  { srcDirs :: [FilePath]
  , immutableSrcDirs :: [FilePath]
  }
  deriving (Show, Eq)

$(deriveJSON Aeson.defaultOptions ''ArboristConfig)

defaultConfigPath :: FilePath
defaultConfigPath = "arborist-config.json"

loadArboristConfig :: Maybe FilePath -> IO ArboristConfig
loadArboristConfig mPath = withFrozenCallStack $ do
  let configPath = fromMaybe defaultConfigPath mPath
  exists <- Dir.doesFileExist configPath
  unless exists $ do
    fail $ "Arborist configuration not found: " <> configPath
  raw <- BS.readFile configPath
  case Aeson.eitherDecodeStrict raw of
    Left err -> fail $ "Unable to parse Arborist configuration: " <> err
    Right rawCfg ->
      let cfg = sanitizeConfig rawCfg
       in if null (allSourceRoots cfg)
            then fail "Arborist configuration must specify at least one visible source root"
            else pure cfg

allSourceRoots :: ArboristConfig -> [FilePath]
allSourceRoots ArboristConfig {srcDirs, immutableSrcDirs} =
  srcDirs ++ immutableSrcDirs

sanitizeConfig :: ArboristConfig -> ArboristConfig
sanitizeConfig ArboristConfig {srcDirs, immutableSrcDirs} =
  ArboristConfig
    { srcDirs = filter (not . null) srcDirs
    , immutableSrcDirs = filter (not . null) immutableSrcDirs
    }
