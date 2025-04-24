{-# LANGUAGE TupleSections #-}

module Arborist.ModGraph (gatherScopeDeps, ProgramIndex, prgsToMap) where

import Arborist.Files
import Arborist.ProgramIndex
import Arborist.Scope.Global
import Control.Error
import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.Foldable
import Data.HashMap.Lazy qualified as Map
import Data.Hashable
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GHC.Stack
import HaskellAnalyzer
import Hir
import Hir.Parse
import Hir.Types
import Hir.Types qualified as Hir
import ModUtils
import System.Directory qualified as Dir

data ModGraph = ModGraph
  { transitiveImports :: Map.HashMap ModuleText (Set.Set ModuleText)
  -- ^ imports as well as re-exported modules and imports
  -- e.g. any module that has an observable name
  }

gatherScopeDeps :: ProgramIndex -> Hir.Program -> ModFileMap -> Maybe Int -> IO (ProgramIndex, ExportIndex)
gatherScopeDeps prgIndex thisPrg modFileMap maxDepth = do
  (prgs, exports) <- indexImports prgIndex thisPrg modFileMap maxDepth
  let prgsWithSelf = fromMaybe prgs ((\mod -> Map.insert mod thisPrg prgs) <$> thisPrg.mod)
  pure (prgsWithSelf, exports)

getRequiredFiles :: ModFileMap -> ModuleText -> [(ModuleText, FilePath)]
getRequiredFiles modFileMap mod =
  maybe [] (List.singleton . (mod,)) (Map.lookup mod modFileMap)

indexImports ::
  ProgramIndex ->
  Hir.Program ->
  ModFileMap ->
  Maybe Int ->
  IO (ProgramIndex, ExportIndex)
indexImports prgs thisPrg modFileMap maxDepth = do
  let requiredFilesWithSrc =
        thisPrg.imports >>= (getRequiredFiles modFileMap) . (.mod)
  -- traceShowM thisPrg.mod
  (importedPrgs, prgs') <- getPrgs prgs requiredFilesWithSrc
  allPrgs <-
    foldl'
      ( \currIdxsM (modText, prg) -> do
          (currPrgs, currExports) <- currIdxsM
          indexImport currPrgs currExports modText prg modFileMap maxDepth
      )
      (pure (prgs', Map.empty))
      importedPrgs

  pure allPrgs

indexImport :: ProgramIndex -> ExportIndex -> ModuleText -> Hir.Program -> ModFileMap -> Maybe Int -> IO (ProgramIndex, ExportIndex)
indexImport prgs exportIndex modText thisPrg modFileMap maxDepth = do
  let addImports = Map.insert modText thisPrg prgs
   in -- indexTransitiveReExports addImports modText thisPrg baseDirs
      indexTransitiveReExports addImports exportIndex modText thisPrg modFileMap maxDepth

-- | Index re-exports transitively, stopping early when all re-exported names are found.
indexTransitiveReExports ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  ModuleText ->
  Hir.Program ->
  ModFileMap ->
  Maybe Int ->
  IO (ProgramIndex, ExportIndex)
indexTransitiveReExports prgs exportIndex modText thisPrg modFileMap maxDepth =
  case thisPrg.exports of
    Nothing ->
      let (_, exportedIndex) = getExportedNames prgs exportIndex modText
       in pure (prgs, exportedIndex)
    Just _exports -> do
      resolveReexports prgs Set.empty [(modText, thisPrg, 0)] modFileMap maxDepth

resolveReexports ::
  (HasCallStack) =>
  ProgramIndex ->
  Set.Set ModuleText -> -- walked reexports
  [(ModuleText, Hir.Program, Int)] -> -- worklist (with per-module depth)
  ModFileMap -> -- base directories to check for haskell files
  Maybe Int -> -- max depth
  IO (ProgramIndex, ExportIndex)
resolveReexports prgs _visited [] _ _ =
  pure (prgs, Map.empty)
resolveReexports prgs visited ((modText, prg, depth) : rest) modFileMap maxDepth
  | Just maxD <- maxDepth
  , depth > maxD =
      resolveReexports prgs visited rest modFileMap maxDepth
  | otherwise = do
      let visited' = Set.insert modText visited

      -- Determine dependencies
      let (_reexportedMods, requiredModules) =
            case prg.exports of
              Nothing -> ([], (.mod) <$> prg.imports)
              Just exports ->
                let exportedMods = (.mod) <$> exportItemMods exports
                    allImportMods = (.mod) <$> prg.imports
                    transitiveNames = getTransitiveReExports prg exports
                    req =
                      if Set.null transitiveNames
                        then exportedMods
                        else allImportMods
                 in (exportedMods, req)

      let requiredFilesWithSrc = requiredModules >>= getRequiredFiles modFileMap

      -- Parse required modules
      (newPrgs, prgs') <- getPrgs prgs requiredFilesWithSrc
      let newWork =
            [ (modText', prg', depth + 1)
            | (modText', prg') <- newPrgs
            , not (Set.member modText' visited')
            ]

      resolveReexports prgs' visited' (newWork ++ rest) modFileMap maxDepth

getPrgs :: ProgramIndex -> [(ModuleText, FilePath)] -> IO ([(ModuleText, Hir.Program)], ProgramIndex)
getPrgs prgs hsFiles =
  foldM step ([], Set.empty, prgs) hsFiles >>= \(newModules, _, updatedMap) -> do
    -- traceShowM $ "getPrgs finished with " <> show (length newModules) <> " new modules"
    pure (newModules, updatedMap)
 where
  step ::
    ([(ModuleText, Program)], Set.Set ModuleText, ProgramIndex) ->
    (ModuleText, FilePath) ->
    IO ([(ModuleText, Program)], Set.Set ModuleText, ProgramIndex)
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
            -- traceShowM $ "Parsing " <> file
            fileContents <- T.decodeUtf8 <$> BS.readFile file
            let (_src, !prg) = parsePrg fileContents
                !parsedList' = (modText, prg) : parsedList
                !nextSeen = Set.insert modText seen
                !nextPrgIndex = Map.insert modText prg prgIndex
            pure (parsedList', nextSeen, nextPrgIndex)
          else do
            pure (parsedList, seen, prgIndex)

insertMany :: (Hashable a) => [(a, b)] -> Map.HashMap a b -> Map.HashMap a b
insertMany toInsert prgs =
  Map.union (Map.fromList toInsert) prgs

safeReadFile :: FilePath -> IO (Maybe T.Text)
safeReadFile path = do
  result <- try (T.readFile path) :: IO (Either SomeException T.Text)
  pure $ either (const Nothing) Just result

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
      (directExportPrgs, prgsWithExports) <- getPrgs prgs requiredFilesWithSrc
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

nubByKey :: (Ord k) => (a -> k) -> [a] -> [a]
nubByKey toKey = go Set.empty
 where
  go _ [] = []
  go !seen (x : xs)
    | Set.member k seen = go seen xs
    | otherwise = x : go (Set.insert k seen) xs
   where
    k = toKey x
