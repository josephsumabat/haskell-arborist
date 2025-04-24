module Arborist.Files where

import Control.Monad (filterM, forM, join)
import Data.HashMap.Lazy qualified as Map
import Data.Maybe
import Hir.Types qualified as Hir
import ModUtils
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

type ModFileMap = Map.HashMap Hir.ModuleText FilePath

-- | Build a map from ModuleText to FilePath for all .hs files under the given baseDirs.
buildModuleFileMap ::
  [FilePath] -> -- base directories
  IO (Map.HashMap Hir.ModuleText FilePath)
buildModuleFileMap baseDirs = do
  filePairsPerDir <- forM baseDirs $ \baseDir -> do
    exists <- doesDirectoryExist baseDir
    if exists
      then do
        hsFiles <- getAllHsFiles [baseDir]
        validPairs <- forM hsFiles $ \file -> do
          case unsafePathToModule [baseDir] file of
            Just modText -> pure (Just (modText, file))
            Nothing -> pure Nothing
        pure (catMaybes validPairs)
      else pure []
  let allFilePairs = concat filePairsPerDir
  pure (Map.fromList allFilePairs)

getAllHsFiles :: [FilePath] -> IO [FilePath]
getAllHsFiles dirs = do
  join
    <$> ( forM dirs $ \dir -> do
            exists <- doesDirectoryExist dir
            if exists
              then getHsFiles dir
              else pure []
        )

-- | Get all .hs files recursively from a given directory
getHsFiles :: FilePath -> IO [FilePath]
getHsFiles dir = do
  contents <- listDirectory dir
  let paths = map (dir </>) contents
  dirs <- filterM doesDirectoryExist paths
  let files = filter (\f -> takeExtension f == ".hs") paths
  subFiles <- fmap concat $ forM dirs getHsFiles
  return (files ++ subFiles)
