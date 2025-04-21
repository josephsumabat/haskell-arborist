module HaskellAnalyzer where

import AST.Cast qualified as AST
import AST.Err qualified as AST
import AST.Haskell qualified as AST
import AST.Node qualified as AST
import AST.Unwrap qualified as AST
import Control.Monad (filterM, forM, join)
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import ModContext
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

getAllHsFiles :: [FilePath] -> IO [FilePath]
getAllHsFiles dirs = do
  join <$> 
    (forM dirs $ \dir -> do
      exists <- doesDirectoryExist dir
      if exists then
        getHsFiles dir
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

parsePrg t = Hir.parseHaskell (AST.parse t)
