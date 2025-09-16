module Arborist.Reexports (
  runReplaceReexports,
  runDeleteEmptyHidingImports,
  runRemoveMercuryPreludeWhenTestPreludePresent,
  runDeleteEmptyImports,
) where

import Arborist.Files
import Arborist.ProgramIndex
import Arborist.Rewrite (rewriteNode)
import Arborist.Rewrite.Apply (writeMultipleEdits)
import Arborist.Scope.Global
import Arborist.Scope.Types
import AST qualified
import AST.Haskell qualified as H
import Control.Monad
import Data.ByteString qualified as BS
import Data.Edit (Edit, delete)
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
import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir
import Hir.Write.Types qualified as Hir.Write
import Text.Pretty.Simple
import Arborist.Refactor.Module (findImportingModules)

-- | Find all modules in the program index that import the given module.
--
-- This function searches through all modules in the program index and checks
-- their import lists to see if they import the specified module.
--
-- @param programIndex The global program index containing all modules
-- @param targetModule The module to search for imports of
-- @return A list of module names that import the target module
-- moved helpers to Arborist.Refactor.Module

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
  let reexports = parseModuleTextFromText <$>
        [
        "Data.Either",

        "Data.Maybe",

        "Model.Email",

        "Mercury.Database.Monad",
        "HspecExtensions",

        "Stubs.Stubs",
        "Stubs.Stubs.S3",
        "Model.Country",
        "Mercury.PersistentUtils",
        "PersistentModels.MercuryAccount",
        "PersistentModels.Organization",
        "Users.PersistentModels.User"
        ]

  let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test"]
  srcFiles <- getAllHsFiles onlySrc
  srcPrgs <- lazyGetPrgs srcFiles
  modFileMap <- buildModuleFileMap onlySrc
  let reexportPrgs = mapMaybe (\m -> Map.lookup m srcPrgs) reexports
  let importingModules = findImportingModules srcPrgs targetReexporting
  let reexportIdentifiers =
        Set.unions $
          flip map reexportPrgs $ \reexportPrg ->
            let glblAvail = getGlobalAvailableDecls srcPrgs Map.empty reexportPrg
                reexportScope = globalDeclsToScope glblAvail reexportPrg.imports
             in Set.fromList $
                  Map.keys reexportScope.glblVarInfo
                    <> Map.keys reexportScope.glblNameInfo
                    <> Map.keys reexportScope.glblConstructorInfo
  case reexportPrgs of
    (prg:_) ->
      let glblAvail = getGlobalAvailableDecls srcPrgs Map.empty prg
       in putStrLn $ TL.unpack . pShowNoColor $ globalDeclsToScope glblAvail prg.imports
    _ -> pure ()
  let edits = replaceReexporters srcPrgs importingModules targetReexporting reexports reexportIdentifiers
      editsWithPaths = mapMaybe (\(mod, e) -> ((\file -> (file, e)) <$> Map.lookup mod modFileMap)) edits
  forM_ editsWithPaths $ \(path, edits) -> writeMultipleEdits path edits
  
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

-- | Find imports that are both hiding and have an empty import list
findEmptyHidingImports :: Hir.Read.Program -> [Hir.Read.Import]
findEmptyHidingImports program =
  filter isEmptyHidingImport program.imports
 where
  isEmptyHidingImport :: Hir.Read.Import -> Bool
  isEmptyHidingImport import_ = 
    import_.hiding && 
    case import_.importList of
      Just [] -> True
      _ -> False

-- | Find imports of A.MercuryTestPrelude that have an empty import list and are not hiding
findEmptyImports :: Hir.Read.Program -> [Hir.Read.Import]
findEmptyImports program =
  filter isEmptyImport program.imports
 where
  isEmptyImport :: Hir.Read.Import -> Bool
  isEmptyImport import_ = 
    import_.mod.text == "A.MercuryTestPrelude" &&
    (not import_.hiding) && 
    case import_.importList of
      Just [] -> True
      _ -> False

-- | Create edits to rewrite empty hiding imports as simple imports
createEmptyHidingImportEdits :: Hir.Read.Program -> [Edit]
createEmptyHidingImportEdits program =
  map rewriteImport (findEmptyHidingImports program)
 where
  rewriteImport :: Hir.Read.Import -> Edit
  rewriteImport import_ = 
    let simpleImport = Hir.Import
          { mod = import_.mod
          , alias = import_.alias
          , qualified = import_.qualified
          , hiding = False  -- Remove hiding
          , importList = Nothing  -- Remove import list
          , dynNode = ()
          }
        newText = Render.renderImport simpleImport
     in rewriteNode import_.dynNode newText

runDeleteEmptyHidingImports :: IO ()
runDeleteEmptyHidingImports = do
  let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test", "../mercury-web-backend/local-packages/a-mercury-prelude/src"]
  srcFiles <- getAllHsFiles onlySrc
  srcPrgs <- lazyGetPrgs srcFiles
  modFileMap <- buildModuleFileMap onlySrc
  
  -- Find all empty hiding imports and create edits
  let allEdits = Map.toList srcPrgs >>= \(modName, program) -> do
        let edits = createEmptyHidingImportEdits program
        case Map.lookup modName modFileMap of
          Just filePath -> [(filePath, edits)]
          Nothing -> []
  
  -- Apply edits to files
  forM_ allEdits $ \(path, edits) -> do
    when (not (null edits)) $ do
      putStrLn $ "Rewriting " ++ show (length edits) ++ " empty hiding imports in " ++ path
      writeMultipleEdits path edits
  
  putStrLn "Finished rewriting empty hiding imports"

-- | Find imports that import both A.MercuryPrelude and A.MercuryTestPrelude
findMercuryPreludeConflicts :: Hir.Read.Program -> [Hir.Read.Import]
findMercuryPreludeConflicts program =
  filter hasMercuryPreludeConflict program.imports
 where
  hasMercuryPreludeConflict :: Hir.Read.Import -> Bool
  hasMercuryPreludeConflict import_ = 
    import_.mod.text == "A.MercuryPrelude" && 
    any (\otherImport -> otherImport.mod.text == "A.MercuryTestPrelude") program.imports

-- | Create edits to remove A.MercuryPrelude imports when A.MercuryTestPrelude is also imported
createMercuryPreludeConflictEdits :: Hir.Read.Program -> [Edit]
createMercuryPreludeConflictEdits program =
  map deleteImport (findMercuryPreludeConflicts program)
 where
  deleteImport :: Hir.Read.Import -> Edit
  deleteImport import_ = delete import_.dynNode.nodeRange

runRemoveMercuryPreludeWhenTestPreludePresent :: IO ()
runRemoveMercuryPreludeWhenTestPreludePresent = do
  let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test", "../mercury-web-backend/local-packages/a-mercury-prelude/src"]
  srcFiles <- getAllHsFiles onlySrc
  srcPrgs <- lazyGetPrgs srcFiles
  modFileMap <- buildModuleFileMap onlySrc
  
  -- Find all conflicting MercuryPrelude imports and create edits
  let allEdits = Map.toList srcPrgs >>= \(modName, program) -> do
        let edits = createMercuryPreludeConflictEdits program
        case Map.lookup modName modFileMap of
          Just filePath -> [(filePath, edits)]
          Nothing -> []
  
  -- Apply edits to files
  forM_ allEdits $ \(path, edits) -> do
    when (not (null edits)) $ do
      putStrLn $ "Removing " ++ show (length edits) ++ " conflicting MercuryPrelude imports in " ++ path
      writeMultipleEdits path edits
  
  putStrLn "Finished removing conflicting MercuryPrelude imports"

-- | Create edits to delete empty imports (not hiding)
createEmptyImportEdits :: Hir.Read.Program -> [Edit]
createEmptyImportEdits program =
  map deleteImport (findEmptyImports program)
 where
  deleteImport :: Hir.Read.Import -> Edit
  deleteImport import_ = delete import_.dynNode.nodeRange

runDeleteEmptyImports :: IO ()
runDeleteEmptyImports = do
  let onlySrc = ["../mercury-web-backend/src", "../mercury-web-backend/test", "../mercury-web-backend/local-packages/a-mercury-prelude/src"]
  srcFiles <- getAllHsFiles onlySrc
  srcPrgs <- lazyGetPrgs srcFiles
  modFileMap <- buildModuleFileMap onlySrc
  
  -- Find all empty imports and create edits
  let allEdits = Map.toList srcPrgs >>= \(modName, program) -> do
        let edits = createEmptyImportEdits program
        case Map.lookup modName modFileMap of
          Just filePath -> [(filePath, edits)]
          Nothing -> []
  
  -- Apply edits to files
  forM_ allEdits $ \(path, edits) -> do
    when (not (null edits)) $ do
      putStrLn $ "Deleting " ++ show (length edits) ++ " empty imports in " ++ path
      writeMultipleEdits path edits
  
  putStrLn "Finished deleting empty imports"
