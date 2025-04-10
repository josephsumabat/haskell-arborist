{-# LANGUAGE TupleSections #-}

module Arborist.ModGraph (gatherScopeDeps, ProgramIndex, prgsToMap) where

import Arborist.ProgramIndex
import Arborist.Scope.Global
import Control.Error
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.HashMap.Lazy qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace
import HaskellAnalyzer
import Hir
import Hir.Types
import Hir.Types qualified as Hir
import ModUtils
import System.Directory qualified as Dir
import System.FilePath qualified as Dir

data ModGraph = ModGraph
  { transitiveImports :: Map.HashMap ModuleText (Set.Set ModuleText)
  -- ^ imports as well as re-exported modules and imports
  -- e.g. any module that has an observable name
  }

gatherScopeDeps :: Hir.Program -> [FilePath] -> IO (ProgramIndex, ExportIndex)
gatherScopeDeps thisPrg baseDirs = do
  res <- indexImports Map.empty thisPrg baseDirs
  pure res

indexImports ::
  ProgramIndex ->
  Hir.Program ->
  [FilePath] ->
  IO (ProgramIndex, ExportIndex)
indexImports prgs thisPrg baseDirs = do
  let requiredFilesWithSrc =
        thisPrg.imports >>= (modWithFiles baseDirs) . (.mod)
  importedPrgs <- getPrgs prgs requiredFilesWithSrc
  allPrgs <-
    foldl'
      ( \currIdxsM (modText, prg) -> do
          (currPrgs, currExports) <- currIdxsM
          indexImport currPrgs currExports modText prg baseDirs
      )
      (pure (prgs, Map.empty))
      importedPrgs
  pure allPrgs

indexImport :: ProgramIndex -> ExportIndex -> ModuleText -> Hir.Program -> [FilePath] -> IO (ProgramIndex, ExportIndex)
indexImport prgs exportIndex modText thisPrg baseDirs = do
  let addImports = Map.insert modText thisPrg prgs
   in -- indexTransitiveReExports addImports modText thisPrg baseDirs
      indexTransitiveReExports addImports exportIndex modText thisPrg baseDirs

-- | Index re-exports transitively, stopping early when all re-exported names are found.
indexTransitiveReExports ::
  ProgramIndex ->
  ExportIndex ->
  ModuleText ->
  Hir.Program ->
  [FilePath] ->
  IO (ProgramIndex, ExportIndex)
indexTransitiveReExports prgs exportIndex modText thisPrg baseDirs =
  case thisPrg.exports of
    Nothing ->
      let (_, exportedIndex) = getExportedNames prgs exportIndex modText
       in pure (prgs, exportedIndex)
    Just exports -> do
      let transitiveReExports = getTransitiveReExports thisPrg exports
      resolveReexports transitiveReExports prgs exportIndex Set.empty [(modText, thisPrg)] baseDirs

-- | Recursively walk the re-export graph, stopping when all required names are resolved.
resolveReexports ::
  -- | Names we still need
  Set.Set T.Text ->
  -- | Indexed programs so far
  ProgramIndex ->
  -- | Cached export info
  ExportIndex ->
  -- | Visited modules (avoid cycles)
  Set.Set ModuleText ->
  -- | Worklist
  [(ModuleText, Hir.Program)] ->
  -- | Base dirs
  [FilePath] ->
  IO (ProgramIndex, ExportIndex)
resolveReexports _remaining prgs exportIdx _visited [] _ = pure (prgs, exportIdx)
resolveReexports remaining prgs exportIdx visited ((modText, prg) : rest) baseDirs
  | Set.null remaining = pure (prgs, exportIdx)
  | Set.member modText visited = resolveReexports remaining prgs exportIdx visited rest baseDirs
  | otherwise = do
      let prgs' = Map.insert modText prg prgs
          visited' = Set.insert modText visited

          -- Export resolution
          (exportedNames, exportIdx') = getExportedNames prgs' exportIdx modText
          found = Set.fromList (map (.name) exportedNames)
          stillMissing = remaining `Set.difference` found

      -- If we still have missing names, explore imports
      let requiredFilesWithSrc =
            case prg.exports of
              Nothing -> []
              Just exports ->
                let exportedMods = (.mod) <$> exportItemMods exports
                    allImportMods = (.mod) <$> prg.imports
                    transitiveReExports = getTransitiveReExports prg exports
                 in if null transitiveReExports
                      then exportedMods >>= modWithFiles baseDirs
                      else allImportMods >>= modWithFiles baseDirs

      newPrgs <- getPrgs prgs' requiredFilesWithSrc

      resolveReexports stillMissing prgs' exportIdx' visited' (newPrgs ++ rest) baseDirs

getPrgs :: ProgramIndex -> [(ModuleText, FilePath)] -> IO [(ModuleText, Hir.Program)]
getPrgs prgs hsFiles = do
  foundPrgs <- fmap (join . catMaybes) $
    forM hsFiles $ \(modText, file) -> do
      let mFound = Map.lookup modText prgs
      case mFound of
        Nothing -> do
          fileExists <-
            Dir.doesFileExist file
          if fileExists
            then do
              fileContents <- T.readFile file
              let (_src, prg) = parsePrg fileContents
              pure $ Just [(modText, prg)]
            else pure Nothing
        Just prg -> pure $ Just [(modText, prg)]
  pure foundPrgs

insertMany :: [(ModuleText, Hir.Program)] -> ProgramIndex -> ProgramIndex
insertMany toInsert prgs =
  foldl'
    (\prgMap (modText, prg) -> Map.insert modText prg prgMap)
    prgs
    toInsert

safeReadFile :: FilePath -> IO (Maybe T.Text)
safeReadFile path = do
  result <- try (T.readFile path) :: IO (Either SomeException T.Text)
  pure $ either (const Nothing) Just result

modWithFiles :: [FilePath] -> ModuleText -> [(ModuleText, FilePath)]
modWithFiles baseDirs modText =
  filesWithSrc (modText, (moduleToPath ".hs" modText))
 where
  filesWithSrc :: (ModuleText, FilePath) -> [(ModuleText, FilePath)]
  filesWithSrc (modText, noSrcPath) =
    (\srcDir -> (modText, srcDir Dir.</> noSrcPath)) <$> baseDirs

-- Unoptimized
indexTransitiveReExportsOld :: ProgramIndex -> ModuleText -> Hir.Program -> [FilePath] -> IO ProgramIndex
indexTransitiveReExportsOld prgs modText thisPrg baseDirs = do
  case thisPrg.exports of
    Nothing -> pure prgs
    Just exports -> do
      let exportedMods = (.mod) <$> mapMaybe getMod exports
          allImportMods = (.mod) <$> thisPrg.imports
          transitiveReExports = getTransitiveReExports thisPrg exports
          requiredFilesWithSrc =
            if null transitiveReExports
              then exportedMods >>= (modWithFiles baseDirs)
              else allImportMods >>= (modWithFiles baseDirs)
      directExportPrgs <- getPrgs prgs requiredFilesWithSrc
      let prgsWithExports = insertMany directExportPrgs prgs
      reachablePrgs <-
        foldl'
          ( \currPrgsM (modText, prg) -> do
              currPrgs <- currPrgsM
              let seen = Map.member modText currPrgs
              if not seen
                then
                  let currPrgs' = Map.insert modText prg currPrgs
                   in indexTransitiveReExportsOld currPrgs' modText prg baseDirs
                else currPrgsM
          )
          (pure prgsWithExports)
          directExportPrgs
      pure reachablePrgs
 where
  getMod :: Hir.ExportItem -> Maybe Hir.ModuleName
  getMod (Hir.ExportModuleItem m) = Just m
  getMod (Hir.ExportItem {}) = Nothing
