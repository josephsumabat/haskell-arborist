module HaskellAnalyzer where

import ModContext
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import qualified AST.Haskell as AST
import qualified AST.Unwrap as AST
import qualified AST.Err as AST
import qualified AST.Node as AST
import qualified AST.Cast as AST
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM)

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
