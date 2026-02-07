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
import Data.List (find, findIndex, nubBy, scanl)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Path qualified as Path
import Data.Pos (Pos (..))
import Data.Range (Range (..))
import Data.Rope qualified as Rope
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Read qualified as T.Read
import Diagnostics (Diagnostic (..), Severity (..))
import Diagnostics.ParseGHC qualified as Diagnostics
import FileWith (FileWith' (..))
import GHC.IO
import HaskellAnalyzer
import Hir.Read.Types qualified as Hir.Read
import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir
import Hir.Write.Types qualified as Hir.Write
import System.Directory qualified as Dir
import System.FilePath qualified as FP

-- local helpers copied from Diagnostics.ParseGHC
readInt :: T.Text -> Maybe Int
readInt t = case T.Read.decimal t of
  Right (i, "") -> Just i
  _ -> Nothing

dec :: Int -> Int
dec x = max 0 (x - 1)

data DiagnosticsEnvironment = DiagnosticsEnvironment
  { diagProjectRoot :: FilePath
  , diagGhcidPath :: FilePath
  , diagMainFile :: FilePath
  }
  deriving (Show, Eq)

normalizeDiagnosticsEnv :: DiagnosticsEnvironment -> IO DiagnosticsEnvironment
normalizeDiagnosticsEnv env = do
  absRoot <- Dir.makeAbsolute env.diagProjectRoot
  pure env {diagProjectRoot = absRoot}

resolveWithinRoot :: DiagnosticsEnvironment -> FilePath -> FilePath
resolveWithinRoot DiagnosticsEnvironment {diagProjectRoot} candidate
  | FP.isAbsolute candidate = candidate
  | otherwise = diagProjectRoot FP.</> candidate

ghcidLogPath :: DiagnosticsEnvironment -> FilePath
ghcidLogPath env = resolveWithinRoot env env.diagGhcidPath

mainFileRelPath :: DiagnosticsEnvironment -> Path.RelPath
mainFileRelPath env =
  let absMain = resolveWithinRoot env env.diagMainFile
      relPath = FP.makeRelative env.diagProjectRoot absMain
   in Path.filePathToRel relPath

diagnosticToAbs :: DiagnosticsEnvironment -> Path.RelPath -> Path.AbsPath
diagnosticToAbs env relPath =
  let relFile = Path.toFilePath relPath
   in Path.unsafeFilePathToAbs (env.diagProjectRoot FP.</> relFile)

loadDiagnostics :: DiagnosticsEnvironment -> IO [Diagnostic]
loadDiagnostics env = do
  info <- catch @IOException (TextIO.readFile (ghcidLogPath env)) (const $ pure "")
  let mainFile = mainFileRelPath env
  pure $ Diagnostics.parse (diagnosticToAbs env) mainFile info

fixRedundantImports :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixRedundantImports env = normalizeDiagnosticsEnv env >>= fixRedundantImportsWithEnv

fixRedundantImportsWithEnv :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixRedundantImportsWithEnv env = do
  diagnostics <- loadDiagnostics env
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

fixNotInScope :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixNotInScope env = normalizeDiagnosticsEnv env >>= fixNotInScopeWithEnv

fixNotInScopeWithEnv :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixNotInScopeWithEnv env = do
  diagnostics <- loadDiagnostics env
  putStrLn $ "Total diagnostics found: " ++ show (length diagnostics)
  -- Debug: print all diagnostic messages
  forM_ diagnostics $ \d -> putStrLn $ "Diagnostic: " ++ T.unpack d.message
  let relevantDiagnostics = filter isNotInScopeDiagnostic diagnostics
  putStrLn $ "Found " ++ show (length relevantDiagnostics) ++ " not-in-scope diagnostics"

  -- Deduplicate diagnostics based on file path, range, and message
  let deduplicatedDiagnostics = nubBy (\d1 d2 -> let FileWith path1 range1 = d1.range; FileWith path2 range2 = d2.range in path1 == path2 && range1 == range2 && d1.message == d2.message) relevantDiagnostics
  putStrLn $ "After deduplication: " ++ show (length deduplicatedDiagnostics) ++ " unique not-in-scope diagnostics"

  -- Collect unique files from deduplicated diagnostics
  let uniqueFiles = Set.fromList [filePath | Diagnostic {range = FileWith filePath _} <- deduplicatedDiagnostics]

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
  -- Local list of imports to insert (deduplicated before insertion)
  importsToInsert :: [Text.Text]
  importsToInsert =
    [ "Test.Mercury.Assertions"
    ]

  collectImportEdit :: Map.HashMap Path.AbsPath [Edit] -> Path.AbsPath -> IO (Map.HashMap Path.AbsPath [Edit])
  collectImportEdit acc filePath = do
    putStrLn $ "Creating import edit for " ++ Path.toFilePath filePath
    -- Create a dummy diagnostic for the file for consistent logging
    let dummyDiagnostic =
          Diagnostic
            { range = FileWith filePath (LineColRange (LineCol (Pos 1) (Pos 1)) (LineCol (Pos 1) (Pos 1)))
            , severity = Error
            , message = ""
            , code = Just "GHC-76037"
            , codeUri = Nothing
            }

    -- Read the file content and parse it to determine insertion point and existing imports
    let absFilePath = Path.toFilePath filePath
    fileContent <- TextIO.readFile absFilePath
    let (_, program) = parsePrg fileContent
        imports = program.imports
        existingMods = Set.fromList (map (\imp -> imp.mod.text) imports)
        uniqueImports = Set.toList (Set.fromList importsToInsert)
        missingImports = filter (\impName -> not (impName `Set.member` existingMods)) uniqueImports
    case imports of
      [] -> do
        -- No imports found; skip to avoid placing imports in the wrong location
        putStrLn $ "No import statements found in " ++ absFilePath ++ ", skipping"
        pure acc
      _ -> do
        -- Find position after the last import
        let pickLast a b = if a.dynNode.nodeRange.end.pos >= b.dynNode.nodeRange.end.pos then a else b
            lastImport = foldl1 pickLast imports
            pos = lastImport.dynNode.nodeRange.end
        if null missingImports
          then do
            putStrLn $ "All desired imports already present in " ++ absFilePath ++ "; skipping insertion"
            pure acc
          else do
            -- Prepare insertion text, ensuring a leading newline if needed
            let lastEndPos = pos.pos
                beforeChar = if lastEndPos > 0 then T.take 1 (T.drop (lastEndPos - 1) fileContent) else T.empty
                prefix = if beforeChar == T.singleton '\n' then T.empty else T.singleton '\n'
                importLines = (\imp -> "import " <> imp) <$> missingImports
                insertionText = prefix <> Text.unlines importLines
                insertionEdit = Edit.insert pos insertionText
            putStrLn $ "Created import insertion edit after last import for " ++ absFilePath
            let existingEdits = Map.findWithDefault [] filePath acc
            pure $ Map.insert filePath (insertionEdit : existingEdits) acc

fixNotExported :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixNotExported env = normalizeDiagnosticsEnv env >>= fixNotExportedWithEnv

fixNotExportedWithEnv :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixNotExportedWithEnv env = do
  diagnostics <- loadDiagnostics env
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

-- | Parse the Nth (0-based) imported-from location from an ambiguous occurrence diagnostic message
parseImportedFromAtIndex :: Int -> T.Text -> Maybe (FilePath, LineColRange)
parseImportedFromAtIndex idx message =
  let lines = T.lines message
      importedLines = filter (T.isInfixOf "imported from") lines
   in case drop idx importedLines of
        (line : _) ->
          let afterAtWithPrefix = snd (T.breakOn " at " line)
           in if T.isPrefixOf " at " afterAtWithPrefix
                then
                  let afterAt = T.drop 4 afterAtWithPrefix
                      trimmed = T.dropWhileEnd (\c -> c == '.' || c == ' ') afterAt
                      (filePart, rest1) = T.breakOn ":" trimmed
                      rest = T.drop 1 rest1
                      (lineStr, rest2) = T.breakOn ":" rest
                      restCols = T.drop 1 rest2
                      (col1Str, col2StrWithDash) = T.breakOn "-" restCols
                      col2Str = T.drop 1 col2StrWithDash
                   in case (readInt lineStr, readInt col1Str, readInt col2Str) of
                        (Just lineNum1, Just col1Num1, Just col2Num) ->
                          let lineNum = dec lineNum1
                              col1Num = dec col1Num1
                              lcr = LineColRange (LineCol (Pos lineNum) (Pos col1Num)) (LineCol (Pos lineNum) (Pos col2Num))
                           in Just (T.unpack filePart, lcr)
                        _ -> Nothing
                else Nothing
        _ -> Nothing

-- | Extract the ambiguous symbol name from the diagnostic message
parseAmbiguousSymbol :: T.Text -> Maybe T.Text
parseAmbiguousSymbol message =
  let (_, rest) = T.breakOn "Ambiguous occurrence ‘" message
   in if T.null rest
        then Nothing
        else
          let after = T.drop (T.length "Ambiguous occurrence ‘") rest
              (sym, _) = T.breakOn "’" after
           in if T.null sym then Nothing else Just sym

-- | Build an ImportItem for a given value name
mkValueImportItem :: T.Text -> Hir.Write.ImportItem
mkValueImportItem nameText =
  let nm =
        Hir.Name
          { nameText = nameText
          , isOperator = False
          , isConstructor = False
          , dynNode = ()
          }
   in Hir.ImportItem
        { namespace = Hir.NameSpaceValue
        , name = nm
        , children = []
        }

-- | Update a write-import to hide the given symbol, or remove it if using include list
hideSymbolInImport :: Hir.Write.Import -> T.Text -> Hir.Write.Import
hideSymbolInImport wimp sym =
  let item = mkValueImportItem sym
   in case (wimp.hiding, wimp.importList) of
        (True, Nothing) ->
          Hir.Import {mod = wimp.mod, alias = wimp.alias, qualified = wimp.qualified, hiding = True, importList = Just [item], dynNode = ()}
        (True, Just items) ->
          let exists i = i.name.nameText == sym
              items' = if any exists items then items else items ++ [item]
           in Hir.Import {mod = wimp.mod, alias = wimp.alias, qualified = wimp.qualified, hiding = True, importList = Just items', dynNode = ()}
        (False, Nothing) ->
          Hir.Import {mod = wimp.mod, alias = wimp.alias, qualified = wimp.qualified, hiding = True, importList = Just [item], dynNode = ()}
        (False, Just items) ->
          let keep i = i.name.nameText /= sym
              items' = filter keep items
           in Hir.Import {mod = wimp.mod, alias = wimp.alias, qualified = wimp.qualified, hiding = False, importList = Just items', dynNode = ()}

-- | Construct a rewrite edit that hides the ambiguous symbol on the selected import (by index)
mkAmbiguousImportHidingEdit :: DiagnosticsEnvironment -> Int -> Diagnostic -> IO (Maybe (FilePath, Range, Edit))
mkAmbiguousImportHidingEdit env whichIdx diagnostic = do
  case (parseImportedFromAtIndex whichIdx diagnostic.message, parseAmbiguousSymbol diagnostic.message) of
    (Just (relFile, lineColRange), Just sym) -> do
      let absFilePath = resolveWithinRoot env relFile
      fileContent <- TextIO.readFile absFilePath
      let rope = Rope.fromText fileContent
          targetRange = Rope.lineColRangeToRange rope lineColRange
          (_, program) = parsePrg fileContent
          imports = program.imports
          contains r1 r2 = r1.start.pos <= r2.start.pos && r1.end.pos >= r2.end.pos
          matched = [imp | imp <- imports, contains imp.dynNode.nodeRange targetRange]
      case matched of
        (imp : _) -> do
          let wimp = Render.fromReadImport imp
              updated = hideSymbolInImport wimp sym
              newText = Render.renderImport updated
              rng = imp.dynNode.nodeRange
              edit = rewriteNode imp.dynNode newText
          pure $ Just (absFilePath, rng, edit)
        _ -> pure Nothing
    _ -> pure Nothing

-- | Fix ambiguous occurrence diagnostics by adding hiding to the second import
fixAmbiguousImports :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixAmbiguousImports env = normalizeDiagnosticsEnv env >>= fixAmbiguousImportsWithEnv

fixAmbiguousImportsWithEnv :: DiagnosticsEnvironment -> IO (Map.HashMap Path.AbsPath [Edit])
fixAmbiguousImportsWithEnv env = do
  diagnostics <- loadDiagnostics env
  putStrLn $ "Total diagnostics found: " ++ show (length diagnostics)
  forM_ diagnostics $ \d -> putStrLn $ "Diagnostic: " ++ T.unpack d.message
  let relevantDiagnostics = filter isAmbiguousImportDiagnostic diagnostics
  putStrLn $ "Found " ++ show (length relevantDiagnostics) ++ " ambiguous import diagnostics"
  let hideSecond = False -- set to False to hide the first import instead
      whichIdx = if hideSecond then 1 else 0
  triplesMap <- foldM (collectTriple env whichIdx) Map.empty relevantDiagnostics
  let editsMap = fmap (map snd) triplesMap
  putStrLn $ "Total files with edits (ambiguous): " ++ show (Map.size editsMap)
  forM_ (Map.toList editsMap) $ \(filePath, edits) -> do
    putStrLn $ "Collected " ++ show (length edits) ++ " ambiguous edits for " ++ Path.toFilePath filePath
  pure editsMap
 where
  collectTriple :: DiagnosticsEnvironment -> Int -> Map.HashMap Path.AbsPath [(Range, Edit)] -> Diagnostic -> IO (Map.HashMap Path.AbsPath [(Range, Edit)])
  collectTriple env' whichIdx acc diagnostic = do
    maybeInfo <- mkAmbiguousImportHidingEdit env' whichIdx diagnostic
    case maybeInfo of
      Just (filePath, rng, edit) -> do
        let absPath = Path.unsafeFilePathToAbs filePath
            existing = Map.findWithDefault [] absPath acc
            already = any (\(r, _) -> r == rng) existing
            newList = if already then existing else (rng, edit) : existing
        pure $ Map.insert absPath newList acc
      Nothing -> pure acc

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

-- | Combine fix maps by appending edits for matching paths.
combineFixMaps :: Map.HashMap Path.AbsPath [Edit] -> Map.HashMap Path.AbsPath [Edit] -> Map.HashMap Path.AbsPath [Edit]
combineFixMaps = Map.unionWith (++)

-- | Run only the simple fixes that don't require extra configuration.
runSimpleFixes :: DiagnosticsEnvironment -> IO ()
runSimpleFixes env = do
  normalized <- normalizeDiagnosticsEnv env
  redundantFixes <- fixRedundantImportsWithEnv normalized
  notExportedFixes <- fixNotExportedWithEnv normalized

  let simpleFixes = redundantFixes `combineFixMaps` notExportedFixes

  putStrLn $ "Combined simple fixes: " ++ show (Map.size simpleFixes) ++ " files with edits"
  applyFixes simpleFixes

-- | Run all fixes and apply them
runAllFixes :: DiagnosticsEnvironment -> IO ()
runAllFixes env = do
  normalized <- normalizeDiagnosticsEnv env
  redundantFixes <- fixRedundantImportsWithEnv normalized
  notInScopeFixes <- fixNotInScopeWithEnv normalized
  notExportedFixes <- fixNotExportedWithEnv normalized
  ambiguousFixes <- fixAmbiguousImportsWithEnv normalized

  -- Combine all fixes
  let allFixes =
        notExportedFixes
          `combineFixMaps` redundantFixes
          `combineFixMaps` ambiguousFixes
          `combineFixMaps` notInScopeFixes

  putStrLn $ "Combined fixes: " ++ show (Map.size allFixes) ++ " files with edits"
  applyFixes allFixes

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
toNewImport orig newMod _reexportIdentifiers =
  let alias = orig.alias
      qualified = orig.qualified
   in [ Hir.Import
          { mod = newMod
          , qualified
          , alias = if isJust alias then alias else Just orig.mod
          , hiding = orig.hiding
          , importList = orig.importList
          , dynNode = ()
          }
      ]

replaceImport :: Hir.Read.Import -> [Hir.ModuleText] -> Set.Set Text.Text -> Edit
replaceImport imp mods reexportIdentifiers =
  let dynNode = imp.dynNode
      renderImp = Render.fromReadImport imp
      newImports = ((\mod -> (toNewImport renderImp mod reexportIdentifiers)) =<< mods)
      finalImports = renderImp : newImports
      newText = Text.unlines $ Render.renderImport <$> finalImports
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

isAmbiguousImportDiagnostic :: Diagnostic -> Bool
isAmbiguousImportDiagnostic diagnostic =
  diagnostic.code == Just "GHC-87543"

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
          _ -> editRange -- Just delete the range as-is
  putStrLn $ "Final deletion range: " ++ show finalRange
  let deletionEdit = Edit.delete finalRange
  putStrLn $ "Created deletion edit for " ++ absFilePath
  pure $ Just (absFilePath, deletionEdit)

mkInsertionInfo :: Diagnostic -> Text.Text -> IO (Maybe (FilePath, Edit))
mkInsertionInfo diagnostic modImport = do
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
      let importStatement = "import " <> modImport <> "\n"
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
