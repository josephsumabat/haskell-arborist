module Diagnostics.Fixes where

import AST qualified
import AST.Haskell qualified as H
import Arborist.ProgramIndex
import Arborist.Rewrite (rewriteNode)
import Arborist.Rewrite.Apply (writeMultipleEdits)
import Control.Applicative (asum)
import Control.Error (catMaybes)
import Control.Exception
import Control.Monad
import Data.Edit as Edit
import Data.HashMap.Lazy qualified as Map
import Data.LineCol (LineCol (..))
import Data.LineColRange (LineColRange (..))
import Data.List (nubBy, find, findIndex, scanl)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Rope qualified as Rope
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Diagnostics (Diagnostic (..), Severity (..))
import Diagnostics.ParseGHC qualified as Diagnostics
import FileWith (FileWith' (..))
import GHC.IO
import HaskellAnalyzer
import Hir.Read.Types qualified as Hir.Read
import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir
import Hir.Write.Types qualified as Hir.Write
import System.FilePath ((</>))

fixRedundantImports :: IO (Map.HashMap Path.AbsPath [Edit])
fixRedundantImports = do
  info <- catch @IOException (TextIO.readFile "/home/josephsumabat/Documents/workspace/mercury-web-backend/ghcid.txt") (const $ pure "")
  let mainFile = "/home/josephsumabat/Documents/workspace/mercury-web-backend/app/main.hs"
  let root = "/home/josephsumabat/Documents/workspace/mercury-web-backend"
  let diagnostics = Diagnostics.parse (Path.unsafeFilePathToAbs . (root System.FilePath.</>) . Path.toFilePath) mainFile info
  putStrLn $ "Total diagnostics found: " ++ show (length diagnostics)
  -- Debug: print all diagnostic messages
  forM_ diagnostics $ \d -> putStrLn $ "Diagnostic: " ++ T.unpack d.message
  let relevantDiagnostics = filter isRedundantImportDiagnostic diagnostics
  putStrLn $ "Found " ++ show (length relevantDiagnostics) ++ " redundant import diagnostics"
  
  -- Deduplicate diagnostics based on file path, range, and message
  let deduplicatedDiagnostics = nubBy (\d1 d2 -> let FileWith path1 range1 = d1.range; FileWith path2 range2 = d2.range in path1 == path2 && range1 == range2 && d1.message == d2.message) relevantDiagnostics
  putStrLn $ "After deduplication: " ++ show (length deduplicatedDiagnostics) ++ " unique redundant import diagnostics"
  
  -- Process each diagnostic and collect edits in a Map
  editsMap <- foldM collectEdit Map.empty deduplicatedDiagnostics
  -- Debug: show what edits are being collected
  putStrLn $ "Total files with edits: " ++ show (Map.size editsMap)
  forM_ (Map.toList editsMap) $ \(filePath, edits) -> do
    putStrLn $ "Collected " ++ show (length edits) ++ " edits for " ++ Path.toFilePath filePath
    forM_ (zip [1 ..] edits) $ \(i, edit) -> do
      putStrLn $ "  Edit " ++ show i ++ ": " ++ show edit
  putStrLn $ "Processed " ++ show (length deduplicatedDiagnostics) ++ " redundant import diagnostics"
  pure editsMap
 where
  collectEdit :: Map.HashMap Path.AbsPath [Edit] -> Diagnostic -> IO (Map.HashMap Path.AbsPath [Edit])
  collectEdit acc diagnostic = do
    maybeEdit <- mkDeletionInfo diagnostic
    case maybeEdit of
      Just (filePath, edit) ->
        let absPath = Path.unsafeFilePathToAbs filePath
            existingEdits = Map.findWithDefault [] absPath acc
         in pure $ Map.insert absPath (edit : existingEdits) acc
      Nothing ->
        pure acc

fixNotInScope :: IO (Map.HashMap Path.AbsPath [Edit])
fixNotInScope = do
  info <- catch @IOException (TextIO.readFile "/home/josephsumabat/Documents/workspace/mercury-web-backend/ghcid.txt") (const $ pure "")
  let mainFile = "/home/josephsumabat/Documents/workspace/mercury-web-backend/app/main.hs"
  let root = "/home/josephsumabat/Documents/workspace/mercury-web-backend"
  let diagnostics = Diagnostics.parse (Path.unsafeFilePathToAbs . (root System.FilePath.</>) . Path.toFilePath) mainFile info
  putStrLn $ "Total diagnostics found: " ++ show (length diagnostics)
  -- Debug: print all diagnostic messages
  forM_ diagnostics $ \d -> putStrLn $ "Diagnostic: " ++ T.unpack d.message
  let relevantDiagnostics = filter isNotInScopeDiagnostic diagnostics
  putStrLn $ "Found " ++ show (length relevantDiagnostics) ++ " not-in-scope diagnostics"
  -- Track processed files to avoid adding multiple imports to the same file
  processedFiles <- forM relevantDiagnostics $ \d -> do
    let FileWith filePath _ = d.range
    pure filePath
  let uniqueFiles = Set.fromList processedFiles
  -- Process each unique file and collect edits in a Map
  editsMap <- foldM collectImportEdit Map.empty (Set.toList uniqueFiles)
  -- Debug: show what edits are being collected
  putStrLn $ "Total files with edits: " ++ show (Map.size editsMap)
  forM_ (Map.toList editsMap) $ \(filePath, edits) -> do
    putStrLn $ "Collected " ++ show (length edits) ++ " edits for " ++ Path.toFilePath filePath
    forM_ (zip [1 ..] edits) $ \(i, edit) -> do
      putStrLn $ "  Edit " ++ show i ++ ": " ++ show edit
  putStrLn $ "Processed " ++ show (Set.size uniqueFiles) ++ " files with not-in-scope diagnostics"
  pure editsMap
 where
  collectImportEdit :: Map.HashMap Path.AbsPath [Edit] -> Path.AbsPath -> IO (Map.HashMap Path.AbsPath [Edit])
  collectImportEdit acc filePath = do
    putStrLn $ "Creating import edit for " ++ Path.toFilePath filePath
    -- Create a dummy diagnostic for the file to reuse mkInsertionInfo
    let dummyDiagnostic =
          Diagnostic
            { range = FileWith filePath (LineColRange (LineCol (Pos 1) (Pos 1)) (LineCol (Pos 1) (Pos 1)))
            , severity = Error
            , message = ""
            , code = Just "GHC-76037"
            , codeUri = Nothing
            }
    maybeEdit <- mkInsertionInfo dummyDiagnostic
    case maybeEdit of
      Just (_, edit) ->
        let existingEdits = Map.findWithDefault [] filePath acc
         in pure $ Map.insert filePath (edit : existingEdits) acc
      Nothing ->
        pure acc

fixNotExported :: IO (Map.HashMap Path.AbsPath [Edit])
fixNotExported = do
  info <- catch @IOException (TextIO.readFile "/home/josephsumabat/Documents/workspace/mercury-web-backend/ghcid.txt") (const $ pure "")
  let mainFile = "/home/josephsumabat/Documents/workspace/mercury-web-backend/app/main.hs"
  let root = "/home/josephsumabat/Documents/workspace/mercury-web-backend"
  let diagnostics = Diagnostics.parse (Path.unsafeFilePathToAbs . (root System.FilePath.</>) . Path.toFilePath) mainFile info
  putStrLn $ "Total diagnostics found: " ++ show (length diagnostics)
  -- Debug: print all diagnostic messages
  forM_ diagnostics $ \d -> putStrLn $ "Diagnostic: " ++ T.unpack d.message
  let relevantDiagnostics = filter isNotExportedDiagnostic diagnostics
  putStrLn $ "Found " ++ show (length relevantDiagnostics) ++ " not-exported diagnostics"
  
  -- Deduplicate diagnostics based on file path, range, and message
  let deduplicatedDiagnostics = nubBy (\d1 d2 -> let FileWith path1 range1 = d1.range; FileWith path2 range2 = d2.range in path1 == path2 && range1 == range2 && d1.message == d2.message) relevantDiagnostics
  putStrLn $ "After deduplication: " ++ show (length deduplicatedDiagnostics) ++ " unique not-exported diagnostics"
  
  -- Process each diagnostic and collect edits in a Map
  editsMap <- foldM collectNotExportedEdit Map.empty deduplicatedDiagnostics
  -- Debug: show what edits are being collected
  putStrLn $ "Total files with edits: " ++ show (Map.size editsMap)
  forM_ (Map.toList editsMap) $ \(filePath, edits) -> do
    putStrLn $ "Collected " ++ show (length edits) ++ " edits for " ++ Path.toFilePath filePath
    forM_ (zip [1 ..] edits) $ \(i, edit) -> do
      putStrLn $ "  Edit " ++ show i ++ ": " ++ show edit
  putStrLn $ "Processed " ++ show (length deduplicatedDiagnostics) ++ " not-exported diagnostics"
  pure editsMap
 where
  collectNotExportedEdit :: Map.HashMap Path.AbsPath [Edit] -> Diagnostic -> IO (Map.HashMap Path.AbsPath [Edit])
  collectNotExportedEdit acc diagnostic = do
    maybeEdit <- mkNotExportedDeletionInfo diagnostic
    case maybeEdit of
      Just (filePath, edit) ->
        let absPath = Path.unsafeFilePathToAbs filePath
            existingEdits = Map.findWithDefault [] absPath acc
         in pure $ Map.insert absPath (edit : existingEdits) acc
      Nothing ->
        pure acc

-- | Apply the collected fixes to the filesystem
applyFixes :: Map.HashMap Path.AbsPath [Edit] -> IO ()
applyFixes fixes = do
  putStrLn $ "Applying fixes to " ++ show (Map.size fixes) ++ " files"
  forM_ (Map.toList fixes) $ \(filePath, edits) -> do
    putStrLn $ "Applying " ++ show (length edits) ++ " edits to " ++ Path.toFilePath filePath
    forM_ (zip [1 ..] edits) $ \(i, edit) -> do
      putStrLn $ "  Edit " ++ show i ++ ": " ++ show edit
    writeMultipleEdits (Path.toFilePath filePath) edits
  putStrLn "All fixes applied successfully"

-- | Run all fixes and apply them
runAllFixes :: IO ()
runAllFixes = do
  redundantFixes <- fixRedundantImports
  --notInScopeFixes <- fixNotInScope
  notExportedFixes <- fixNotExported

  -- Combine all fixes
  --let allFixes = Map.unionWith (++) redundantFixes (Map.unionWith (++) notInScopeFixes notExportedFixes)
  let allFixes = notExportedFixes `combine` redundantFixes

  putStrLn $ "Combined fixes: " ++ show (Map.size allFixes) ++ " files with edits"
  applyFixes allFixes
  where
    combine = Map.unionWith (++)

replaceReexporters :: ProgramIndex -> [Hir.ModuleText] -> Hir.ModuleText -> [Hir.ModuleText] -> Set.Set Text.Text -> [(Hir.ModuleText, [Edit])]
replaceReexporters prgIndex importingMods reexportingMod reexportedMods reexportIdentifiers =
  mapMaybe
    ( \importingMod ->
        let mPrg = Map.lookup importingMod prgIndex
         in (\prg -> (importingMod, replaceModImports prg reexportingMod reexportedMods reexportIdentifiers)) <$> mPrg
    )
    importingMods

replaceModImports :: Hir.Read.Program -> Hir.ModuleText -> [Hir.ModuleText] -> Set.Set Text.Text -> [Edit]
replaceModImports prg targetMod modsToAdd reexportIdentifiers =
  let imports = prg.imports
      foundImports = filter (\imp -> imp.mod == targetMod) imports
   in (\foundImport -> replaceImport foundImport modsToAdd reexportIdentifiers) <$> foundImports

toNewImport :: Hir.Write.Import -> Hir.ModuleText -> Set.Set Text.Text -> [Hir.Write.Import]
toNewImport orig newMod reexportIdentifiers =
  let alias = orig.alias
      importList = filter (\importItem -> Set.member importItem.name.nameText reexportIdentifiers) <$> orig.importList
      hiding =
        case importList of
          Nothing -> False
          Just [] -> False
          Just _xs -> orig.hiding
      qualified = orig.qualified
   in [ orig
      , Hir.Import
          { mod = newMod
          , qualified
          , alias
          , hiding = orig.hiding
          , importList = orig.importList
          , dynNode = ()
          }
      ]

replaceImport :: Hir.Read.Import -> [Hir.ModuleText] -> Set.Set Text.Text -> Edit
replaceImport imp mods reexportIdentifiers =
  let dynNode = imp.dynNode
      renderImp = Render.fromReadImport imp
      newText = Text.unlines $ Render.renderImport <$> (((\mod -> (toNewImport renderImp mod reexportIdentifiers)) =<< mods))
   in rewriteNode dynNode newText

-- | Check if a diagnostic is about a redundant import
isRedundantImportDiagnostic :: Diagnostic -> Bool
isRedundantImportDiagnostic diagnostic =
  diagnostic.code == Just "GHC-66111"

isNotInScopeDiagnostic :: Diagnostic -> Bool
isNotInScopeDiagnostic diagnostic =
  diagnostic.code == Just "GHC-76037"
    || diagnostic.code == Just "GHC-88464"

isNotExportedDiagnostic :: Diagnostic -> Bool
isNotExportedDiagnostic diagnostic =
  diagnostic.code == Just "GHC-61689"

-- | Convert a diagnostic to a deletion edit that removes the redundant import line
mkDeletionInfo :: Diagnostic -> IO (Maybe (FilePath, Edit))
mkDeletionInfo diagnostic = do
  let FileWith filePath range = diagnostic.range
  let absFilePath = Path.toFilePath filePath
  -- Debug: print the diagnostic range
  putStrLn $ "Processing diagnostic: " ++ show range ++ " in " ++ absFilePath
  -- Read the file content to calculate proper character positions
  fileContent <- TextIO.readFile absFilePath
  -- Convert LineColRange to Range for Edit operations
  let editRange = Rope.lineColRangeToRange (Rope.fromText fileContent) range
  putStrLn $ "Converted to Range: " ++ show editRange
  -- Create a deletion edit
  let deletionEdit = Edit.delete editRange
  putStrLn $ "Created deletion edit for " ++ absFilePath
  pure $ Just (absFilePath, deletionEdit)

-- | Convert a diagnostic to a deletion edit that removes the not-exported item and optionally a following comma
mkNotExportedDeletionInfo :: Diagnostic -> IO (Maybe (FilePath, Edit))
mkNotExportedDeletionInfo diagnostic = do
  let FileWith filePath range = diagnostic.range
  let absFilePath = Path.toFilePath filePath
  -- Debug: print the diagnostic range
  putStrLn $ "Processing not-exported diagnostic: " ++ show range ++ " in " ++ absFilePath
  putStrLn $ "Diagnostic message: " ++ T.unpack diagnostic.message
  
  -- Read the file content to calculate proper character positions
  fileContent <- TextIO.readFile absFilePath
  let rope = Rope.fromText fileContent
  -- Convert LineColRange to Range for Edit operations
  let editRange = Rope.lineColRangeToRange rope range
  putStrLn $ "Converted to Range: " ++ show editRange
  
  -- Debug: show the exact content being deleted
  let deletedContent = Rope.indexRange rope editRange
      deletedText = maybe "" Rope.toText deletedContent
  putStrLn $ "Content to be deleted: '" ++ T.unpack deletedText ++ "'"
  
  -- Check if there's a comma after the range
  let endPos = editRange.end
      -- Look for a comma in the next few characters after the range
      searchRange = Range endPos (endPos {pos = endPos.pos + 10})
      afterContent = Rope.indexRange rope searchRange
      afterText = maybe "" Rope.toText afterContent
  putStrLn $ "Content after range: '" ++ T.unpack afterText ++ "'"
  
  -- Check if there's a comma immediately after the item
  let finalRange =
        case T.uncons afterText of
          Just (',', _) ->
            -- Include the comma in the deletion
            Range editRange.start (endPos {pos = endPos.pos + 1})
          _ -> editRange  -- Just delete the range as-is
  
  putStrLn $ "Final deletion range: " ++ show finalRange
  let deletionEdit = Edit.delete finalRange
  putStrLn $ "Created deletion edit for " ++ absFilePath
  pure $ Just (absFilePath, deletionEdit)

mkInsertionInfo :: Diagnostic -> IO (Maybe (FilePath, Edit))
mkInsertionInfo diagnostic = do
  let FileWith filePath range = diagnostic.range
  let absFilePath = Path.toFilePath filePath
  -- Debug: print the diagnostic range
  putStrLn $ "Processing not-in-scope diagnostic: " ++ show range ++ " in " ++ absFilePath
  -- Read the file content and parse it
  fileContent <- TextIO.readFile absFilePath
  let (_, program) = parsePrg fileContent
  -- Find the first import statement in the AST
  let firstImportPos = findFirstImportPosition program.node.dynNode
  case firstImportPos of
    Just pos -> do
      putStrLn $ "Found first import at position: " ++ show pos
      -- Create an insertion edit with a sample import statement
      let importStatement = "import TestImport.Import.NoFoundation\n"
      let insertionEdit = Edit.insert pos importStatement
      putStrLn $ "Created import insertion edit for " ++ absFilePath
      pure $ Just (absFilePath, insertionEdit)
    Nothing -> do
      putStrLn $ "No import statements found in " ++ absFilePath ++ ", skipping"
      pure Nothing

-- | Find the position of the first import statement in the AST
findFirstImportPosition :: AST.DynNode -> Maybe Pos
findFirstImportPosition node = go node
 where
  go n =
    case AST.cast @H.ImportP n of
      Just _ -> Just n.nodeRange.start
      Nothing -> asum (go <$> n.nodeChildren)
