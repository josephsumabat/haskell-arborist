module Arborist.Reexports (
  runReplaceReexports,
) where

import Arborist.Files
import Arborist.ProgramIndex
import Arborist.Rewrite (writeMultipleEdits)
import Arborist.Scope.Global
import Arborist.Scope.Types
import Control.Monad
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Diagnostics.Fixes
import GHC.IO
import HaskellAnalyzer
import Hir.Parse
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import Text.Pretty.Simple

-- | Find all modules in the program index that import the given module.
--
-- This function searches through all modules in the program index and checks
-- their import lists to see if they import the specified module.
--
-- @param programIndex The global program index containing all modules
-- @param targetModule The module to search for imports of
-- @return A list of module names that import the target module
findImportingModules :: ProgramIndex -> Hir.ModuleText -> [Hir.ModuleText]
findImportingModules programIndex targetModule =
  Map.keys $ Map.filter hasImport programIndex
 where
  hasImport :: Hir.Program hirKind -> Bool
  hasImport program = any (\import_ -> import_.mod == targetModule) program.imports

-- | Helper function to extract module names from export items
exportItemMods :: [Hir.ExportItem hirKind] -> [Hir.ModuleName]
exportItemMods exports =
  mapMaybe exportItemToMod exports
 where
  exportItemToMod :: Hir.ExportItem hirKind -> Maybe Hir.ModuleName
  exportItemToMod (Hir.ExportModuleItem mod) = Just mod
  exportItemToMod _ = Nothing

runReplaceReexports :: IO ()
runReplaceReexports = do
  let targetReexporting = parseModuleTextFromText "TestImport"
  let reexport = parseModuleTextFromText "TestImport.Import.NoFoundation"
  let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test", "../mercury-web-backend/local-packages/a-mercury-prelude/src"]
  srcFiles <- getAllHsFiles onlySrc
  srcPrgs <- lazyGetPrgs srcFiles
  modFileMap <- buildModuleFileMap onlySrc
  let reexportPrg = fromJust (Map.lookup reexport srcPrgs)
  let importingModules = findImportingModules srcPrgs targetReexporting
  putStrLn $ "Modules importing TestImport" ++ show importingModules
  let glblAvail = getGlobalAvailableDecls srcPrgs Map.empty (fromJust $ Map.lookup reexport srcPrgs)
  let reexportScope = globalDeclsToScope glblAvail reexportPrg.imports
  let reexportIdentifiers =
        Set.fromList $
          Map.keys reexportScope.glblVarInfo
            <> Map.keys reexportScope.glblNameInfo
            <> Map.keys reexportScope.glblConstructorInfo
  putStrLn $ TL.unpack . pShowNoColor $ globalDeclsToScope glblAvail reexportPrg.imports
  let edits = replaceReexporters srcPrgs importingModules targetReexporting [reexport] reexportIdentifiers
      editsWithPaths = mapMaybe (\(mod, e) -> ((\file -> (file, e)) <$> Map.lookup mod modFileMap)) edits
  forM_ editsWithPaths $ \(path, edits) -> writeMultipleEdits path edits
 where
  lazyGetPrgs :: [FilePath] -> IO ProgramIndex
  lazyGetPrgs hsFiles = do
    -- traceShowM hsFiles
    entries <- forM hsFiles $ \file -> do
      prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
      pure (file, prog)
    pure $ Map.fromList $ mapMaybe (\(_, prg) -> (,prg) <$> prg.mod) entries
  parseFile :: [Char] -> IO Hir.Read.Program
  parseFile file = do
    -- traceShowM $ "parsing: " <> file
    fileContents <- fmap TE.decodeUtf8 . BS.readFile $ file
    pure $ snd (parsePrg fileContents) -- Run `test` function
