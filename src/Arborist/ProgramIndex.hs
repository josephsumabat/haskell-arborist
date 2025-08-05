{-# LANGUAGE TupleSections #-}

module Arborist.ProgramIndex (
  gatherScopeDeps,
  getPrgs,
  ProgramIndex,
  prgsToMap,
) where

import Arborist.Exports
import Arborist.Files
import Control.Error
import Control.Monad
import Data.ByteString qualified as BS
import Data.Foldable
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import GHC.Stack
import HaskellAnalyzer
import Hir
import Hir.Read.Types qualified as Hir.Read
import Hir.Types
import System.Directory qualified as Dir

-- | In memory index of module -> program
type ProgramIndex = Map.HashMap ModuleText Hir.Read.Program

-- | Find all dependencies required to resolve a given module and add them to a `ProgramIndex`
-- Optionally accepts a maximum depth to search dependencies for
gatherScopeDeps :: ProgramIndex -> Hir.Read.Program -> ModFileMap -> Maybe Int -> IO ProgramIndex
gatherScopeDeps prgIndex thisPrg modFileMap maxDepth = do
  prgs <- indexImports prgIndex thisPrg modFileMap maxDepth
  let prgsWithSelf = fromMaybe prgs ((\mod -> Map.insert mod thisPrg prgs) <$> thisPrg.mod)
  pure prgsWithSelf

-- | Helper to look up a list of modules
getModFiles :: ModFileMap -> [ModuleText] -> [(ModuleText, FilePath)]
getModFiles modFileMap mods =
  mods >>= \mod -> maybe [] (List.singleton . (mod,)) (Map.lookup mod modFileMap)

-- | Index all imports of a given program
indexImports ::
  ProgramIndex ->
  Hir.Read.Program ->
  ModFileMap ->
  Maybe Int ->
  IO ProgramIndex
indexImports prgs thisPrg modFileMap maxDepth = do
  let imports = getImports thisPrg
      requiredFilesWithSrc =
        getModFiles modFileMap ((.mod) <$> imports)
  (importedPrgs, prgs') <- getPrgs prgs requiredFilesWithSrc
  allPrgs <-
    foldl'
      ( \currIdxsM (modText, prg) -> do
          currPrgs <- currIdxsM
          indexImport currPrgs modText prg modFileMap maxDepth
      )
      (pure prgs')
      importedPrgs

  pure allPrgs

indexImport :: ProgramIndex -> ModuleText -> Hir.Read.Program -> ModFileMap -> Maybe Int -> IO (ProgramIndex)
indexImport prgs modText thisPrg modFileMap maxDepth = do
  let addImports = Map.insert modText thisPrg prgs
   in indexTransitiveReExports addImports modText thisPrg modFileMap maxDepth

-- | Index re-exports transitively, stopping early when all re-exported names are found.
indexTransitiveReExports ::
  (HasCallStack) =>
  ProgramIndex ->
  ModuleText ->
  Hir.Read.Program ->
  ModFileMap ->
  Maybe Int ->
  IO (ProgramIndex)
indexTransitiveReExports prgs modText thisPrg modFileMap maxDepth =
  case thisPrg.exports of
    Nothing -> pure prgs
    Just _exports -> do
      resolveReexports prgs Set.empty [(modText, thisPrg, 0)] modFileMap maxDepth

-- | Does a DFS
resolveReexports ::
  (HasCallStack) =>
  ProgramIndex ->
  Set.Set ModuleText -> -- walked reexports
  [(ModuleText, Hir.Read.Program, Int)] -> -- worklist (with per-module depth)
  ModFileMap -> -- base directories to check for haskell files
  Maybe Int -> -- max depth
  IO (ProgramIndex)
resolveReexports prgs _visited [] _ _ =
  pure prgs
resolveReexports prgs visited ((modText, prg, depth) : rest) modFileMap maxDepth
  | Just maxD <- maxDepth
  , depth >= maxD =
      resolveReexports prgs visited rest modFileMap maxDepth
  | otherwise = do
      let visited' = Set.insert modText visited

      -- Determine dependencies
      let (_reexportedMods, requiredModules) =
            let imports = getImports prg
             in case prg.exports of
                  Nothing -> ([], (.mod) <$> imports)
                  Just exports ->
                    let
                      reExportedAliases = (.mod) <$> exportItemMods exports
                      aliasModMap = getAliasModMap prg
                      reExportedMods = modsFromAliases aliasModMap reExportedAliases
                      allImportMods = (.mod) <$> imports
                      exportedMods = (.mod) <$> filter (isExportedImport reExportedMods) imports
                      transitiveNames = getTransitiveReExportNames prg exports
                      req =
                        if Set.null transitiveNames
                          then exportedMods
                          else allImportMods
                     in
                      (exportedMods, req)

      let requiredFilesWithSrc = getModFiles modFileMap requiredModules

      -- Parse required modules
      (newPrgs, prgs') <- getPrgs prgs requiredFilesWithSrc
      let newWork =
            [ (modText', prg', depth + 1)
            | (modText', prg') <- newPrgs
            , not (Set.member modText' visited')
            ]

      resolveReexports prgs' visited' (newWork ++ rest) modFileMap maxDepth

-- | Return the parsed representation for each module in a given list of modules
-- also indexes the parsed module such that each module is never re-parsed
getPrgs :: ProgramIndex -> [(ModuleText, FilePath)] -> IO ([(ModuleText, Hir.Read.Program)], ProgramIndex)
getPrgs prgs hsFiles =
  foldM step ([], Set.empty, prgs) hsFiles >>= \(newModules, _, updatedMap) -> do
    pure (newModules, updatedMap)
 where
  step ::
    ( [(ModuleText, Hir.Read.Program)] -- List of requested modules
    , Set.Set ModuleText -- Modules that we have already added to the list
    , ProgramIndex -- Updated program index
    ) ->
    (ModuleText, FilePath) -> -- Module to attempt to lookup
    IO ([(ModuleText, Hir.Read.Program)], Set.Set ModuleText, ProgramIndex)
  step (!parsedList, !seen, !prgIndex) (modText, file) =
    case (Map.lookup modText prgIndex, Set.member modText seen) of
      (_, True) -> pure (parsedList, seen, prgIndex)
      (Just prg, False) -> do
        let !parsedList' = (modText, prg) : parsedList
        pure (parsedList', Set.insert modText seen, prgIndex)
      (Nothing, False) -> do
        fileExists <- Dir.doesFileExist file
        if fileExists
          then do
            -- traceShowMPretty file
            fileContents <- T.decodeUtf8 <$> BS.readFile file
            let (_src, !prg) = parsePrg fileContents
                !parsedList' = (modText, prg) : parsedList
                !nextSeen = Set.insert modText seen
                !nextPrgIndex = Map.insert modText prg prgIndex
            pure (parsedList', nextSeen, nextPrgIndex)
          else do
            pure (parsedList, seen, prgIndex)

prgsToMap :: [Hir.Read.Program] -> ProgramIndex
prgsToMap prgs =
  Map.fromList $
    mapMaybe
      ( \prg ->
          (,prg) <$> prg.mod
      )
      prgs
