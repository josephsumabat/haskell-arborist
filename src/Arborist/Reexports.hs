module Arborist.Reexports (
  runReplaceReexports,
  replaceImport,
) where

import Arborist.Rewrite
import Arborist.Files
import Hir.Render.Import qualified as Render
import Arborist.ProgramIndex
import Arborist.Rewrite (rewriteNode)
import Data.HashMap.Lazy qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Hir.Types qualified as Hir
import Hir.Types qualified as HirImport (Import (..))
import Data.Edit as Edit (Edit, empty)
import Data.Text qualified as Text
import Data.List (find)
import AST qualified
import AST.Haskell qualified as H
import Hir.Parse
import GHC.IO
import Control.Monad
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import HaskellAnalyzer

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
  hasImport :: Hir.Program -> Bool
  hasImport program = any (\import_ -> import_.mod == targetModule) program.imports

-- | Helper function to extract module names from export items
exportItemMods :: [Hir.ExportItem] -> [Hir.ModuleName]
exportItemMods exports =
  mapMaybe exportItemToMod exports
 where
  exportItemToMod :: Hir.ExportItem -> Maybe Hir.ModuleName
  exportItemToMod (Hir.ExportModuleItem mod) = Just mod
  exportItemToMod _ = Nothing 

runReplaceReexports :: IO ()
runReplaceReexports = do
    let targetReexporting = parseModuleTextFromText "TestImport"
    let reexport = parseModuleTextFromText "TestImport.Import.NoFoundation"
    let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test"]
    srcFiles <- getAllHsFiles onlySrc
    srcPrgs <- lazyGetPrgs srcFiles
    modFileMap <- buildModuleFileMap onlySrc
    let importingModules = findImportingModules srcPrgs targetReexporting
    let edits = replaceReexporters srcPrgs importingModules targetReexporting [reexport]
        editsWithPaths = mapMaybe (\(mod, e) -> ((\file -> (file, e)) <$> Map.lookup mod modFileMap)) edits
    putStrLn $ "Modules importing TestImport" ++ show importingModules
    forM_ editsWithPaths $ \(path, edits) -> writeMultipleEdits path edits
      where
  lazyGetPrgs :: [FilePath] -> IO ProgramIndex
  lazyGetPrgs hsFiles = do
    -- traceShowM hsFiles
    entries <- forM hsFiles $ \file -> do
      prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
      pure (file, prog)
    pure $ Map.fromList $ mapMaybe (\(_, prg) -> (,prg) <$> prg.mod) entries
  parseFile :: [Char] -> IO Hir.Program
  parseFile file = do
    -- traceShowM $ "parsing: " <> file
    fileContents <- fmap TE.decodeUtf8 . BS.readFile $ file
    pure $ snd (parsePrg fileContents) -- Run `test` function

replaceReexporters :: ProgramIndex -> [Hir.ModuleText] -> Hir.ModuleText -> [Hir.ModuleText] -> [(Hir.ModuleText, [Edit])]
replaceReexporters prgIndex importingMods reexportingMod reexportedMods =
  mapMaybe (\importingMod ->
    let mPrg = Map.lookup importingMod prgIndex in
        (\prg -> (importingMod, replaceModImports prg reexportingMod reexportedMods)) <$> mPrg
    )  importingMods

replaceModImports :: Hir.Program -> Hir.ModuleText -> [Hir.ModuleText] -> [Edit]
replaceModImports prg targetMod modsToAdd =
  let imports = prg.imports
      foundImports = filter (\imp -> imp.mod == targetMod) imports
  in 
    (\foundImport -> replaceImport foundImport modsToAdd) <$> foundImports

toImport :: Hir.Import -> Hir.ModuleText -> Hir.Import
toImport orig newMod = 
        orig { HirImport.mod = newMod }

replaceImport :: Hir.Import -> [Hir.ModuleText] -> Edit
replaceImport imp mods = 
  let dynNode = imp.dynNode
      newText = Text.unlines $ Render.renderImport <$> Render.fromHirImport imp:((\mod -> Render.fromHirImport (toImport imp mod)) <$> mods)
  in rewriteNode dynNode newText 
