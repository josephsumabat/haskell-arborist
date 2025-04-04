{-# LANGUAGE TupleSections #-}
module Arborist.ModGraph (gatherScopeDeps, ProgramIndex, prgsToMap) where

import Arborist.Scope.Types
import Control.Error
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.HashMap.Lazy qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Debug.Trace
import HaskellAnalyzer
import Hir.Types (ModuleText)
import Hir.Types qualified as Hir
import ModUtils
import System.Directory qualified as Dir
import System.FilePath qualified as Dir

data ModGraph = ModGraph
  { transitiveImports :: Map.HashMap ModuleText (Set.Set ModuleText)
  -- ^ imports as well as re-exported modules and imports
  -- e.g. any module that has an observable name
  }

-- | In memory index of module -> program
type ProgramIndex = Map.HashMap ModuleText Hir.Program

prgsToMap :: [Hir.Program] -> ProgramIndex
prgsToMap prgs =
  Map.fromList $
    mapMaybe
      ( \prg ->
          (,prg) <$> prg.mod
      )
      prgs

gatherScopeDeps :: Hir.Program -> [FilePath] -> IO ProgramIndex
gatherScopeDeps thisPrg baseDirs = do
  allPrgs <- indexImports Map.empty thisPrg baseDirs
  pure allPrgs

indexImports :: ProgramIndex -> Hir.Program -> [FilePath] -> IO ProgramIndex
indexImports prgs thisPrg baseDirs = do
  let requiredFilesWithSrc =
        thisPrg.imports >>= (modWithFiles baseDirs) . (.mod)
  importedPrgs <- getPrgs prgs requiredFilesWithSrc
  allPrgs <-
    foldl'
      ( \currPrgsM (modText, prg) -> do
          currPrgs <- currPrgsM
          indexImport currPrgs modText prg baseDirs
      )
      (pure prgs)
      importedPrgs
  pure allPrgs

indexImport :: ProgramIndex -> ModuleText -> Hir.Program -> [FilePath] -> IO ProgramIndex
indexImport prgs modText thisPrg baseDirs = do
  let addImports = Map.insert modText thisPrg prgs
  indexTransitiveReExports addImports modText thisPrg baseDirs

indexTransitiveReExports :: ProgramIndex -> ModuleText -> Hir.Program -> [FilePath] -> IO ProgramIndex
indexTransitiveReExports prgs modText thisPrg baseDirs = do
  case thisPrg.exports of
    Nothing -> pure prgs
    Just exports -> do
      let exportedMods = (.mod) <$> mapMaybe getMod exports
          requiredFilesWithSrc =
            exportedMods >>= (modWithFiles baseDirs)
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
                   in indexTransitiveReExports currPrgs' modText prg baseDirs
                else currPrgsM
          )
          (pure prgsWithExports)
          directExportPrgs
      pure reachablePrgs
 where
  getMod :: Hir.ExportItem -> Maybe Hir.ModuleName
  getMod (Hir.ExportModuleItem m) = Just m
  getMod (Hir.ExportItem {}) = Nothing

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
              traceShowM $ "parsing: " <> file
              fileContents <- Utf8.readFile file
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
  result <- try (Utf8.readFile path) :: IO (Either SomeException T.Text)
  pure $ either (const Nothing) Just result

modWithFiles :: [FilePath] -> ModuleText -> [(ModuleText, FilePath)]
modWithFiles baseDirs modText =
  filesWithSrc (modText, (moduleToPath ".hs" modText))
 where
  filesWithSrc :: (ModuleText, FilePath) -> [(ModuleText, FilePath)]
  filesWithSrc (modText, noSrcPath) =
    (\srcDir -> (modText, srcDir Dir.</> noSrcPath)) <$> baseDirs
