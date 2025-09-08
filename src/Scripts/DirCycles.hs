{-# LANGUAGE TupleSections #-}

-- | Directory Cycle Detection and Breaking Tool
-- 
-- This module provides functionality to:
-- 1. Detect dependency cycles at the directory level
-- 2. Analyze which modules could be moved to break cycles
-- 3. Find the minimum set of modules to move to break all cycles
-- 4. Provide detailed analysis and suggestions for refactoring
--
-- Usage:
-- - runDetectCycles: Uses hardcoded configuration
-- - runDetectCyclesWithConfig: Custom configuration
-- - main: Command line interface

module Scripts.DirCycles where

import Arborist.Files
import Arborist.ProgramIndex
import Arborist.Refactor.Module (renameModule)
import Arborist.Rewrite (applySourceEdit)
import Data.SourceEdit (SourceEdit)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Data.Text qualified as T
import Hir.Parse
import Hir.Parse (parseModuleTextFromText)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import HaskellAnalyzer
import Data.Graph (SCC(..), stronglyConnComp)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.FilePath qualified as FP
import System.Directory qualified as Dir
import ModUtils (moduleToPath)

-- Keep minimal types needed for this script
data Cycle = Cycle { cyclePath :: [FilePath], cycleStart :: FilePath } deriving (Show, Eq, Ord)

-- | Represents a module dependency graph
data ModuleGraph = ModuleGraph
  { graphNodes :: Map.HashMap Hir.ModuleText ModuleNode
  , graphEdges :: Map.HashMap Hir.ModuleText (Set.Set Hir.ModuleText)  -- Module -> Set of modules it imports
  }
  deriving (Show)

-- | Represents a node in the module graph
data ModuleNode = ModuleNode
  { moduleName :: Hir.ModuleText
  , moduleDirectory :: FilePath
  , moduleImports :: [Hir.ModuleText]
  }
  deriving (Show, Eq)

-- | Intermediate representation for pretty-printing a path of modules
newtype ModulePathView = ModulePathView { moduleSequence :: [Hir.ModuleText] }
  deriving (Eq, Ord, Show)

-- | Render a module path like "A.B -> C.D -> E.F"
render :: ModulePathView -> String
render (ModulePathView mods) =
  "  " <> List.intercalate " -> " (map show mods)

-- | Build module graph from program index
buildModuleGraph :: ProgramIndex -> ModuleGraph
buildModuleGraph programIndex =
  let nodes = Map.mapWithKey (\moduleName program ->
        ModuleNode
          { moduleName = moduleName
          , moduleDirectory = moduleToDirectory moduleName
          , moduleImports = (.mod) <$> program.imports
          }
        ) programIndex
      
      -- Build edges: module -> set of modules it imports
      edges = Map.mapWithKey (\moduleName program ->
        Set.fromList $ (.mod) <$> program.imports
        ) programIndex
   in ModuleGraph
        { graphNodes = nodes
        , graphEdges = edges
        }

-- | Convert a module to its directory path (relative), e.g. A.B.C -> A/B
moduleToDirectory :: Hir.ModuleText -> FilePath
moduleToDirectory modText = takeDirectory (moduleToPath ".hs" modText)

-- | Parse a Haskell module file into a Program (read-only AST)
parseProgramFromFile :: FilePath -> IO Hir.Read.Program
parseProgramFromFile file = do
  contents <- TE.decodeUtf8 <$> BS.readFile file
  let (_src, prg) = parsePrg contents
  pure prg

-- | Build a module graph from all modules under the given source directories
buildModuleGraphFromSourceDirs :: [FilePath] -> IO ModuleGraph
buildModuleGraphFromSourceDirs sourceDirs = do
  modFileMap <- buildModuleFileMap sourceDirs
  prgPairs <- forM (Map.toList modFileMap) $ \(modText, file) -> do
    prg <- parseProgramFromFile file
    pure (modText, prg)
  let programIndex = Map.fromList prgPairs
  pure (buildModuleGraph programIndex)

-- | Build a directory-level dependency graph from the module graph
-- Each node is a directory, edges represent dependencies between directories
buildDirectoryGraph :: ModuleGraph -> Map.HashMap FilePath (Set.Set FilePath)
buildDirectoryGraph mg =
  let edges = mg.graphEdges
      getDir :: Hir.ModuleText -> FilePath
      getDir m = case Map.lookup m mg.graphNodes of
        Just n -> n.moduleDirectory
        Nothing -> moduleToDirectory m
      addEdges acc (modName, importSet) =
        let fromDir = getDir modName
            toDirs = Set.map getDir importSet
            existing = Map.lookupDefault Set.empty fromDir acc
         in Map.insert fromDir (Set.union existing toDirs) acc
   in Map.foldlWithKey' (\acc k v -> addEdges acc (k, v)) Map.empty edges

-- | Find SCCs (cycles) in the directory graph
dirSCCs :: Map.HashMap FilePath (Set.Set FilePath) -> [SCC FilePath]
dirSCCs dirGraph =
  let allNodes =
        Set.toList $
          Set.union
            (Set.fromList (Map.keys dirGraph))
            (Set.unions (Map.elems dirGraph))
      depsOf n = Set.toList (Map.lookupDefault Set.empty n dirGraph)
      sccInput = [ (n, n, depsOf n) | n <- allNodes ]
   in stronglyConnComp sccInput

-- | Extract one simple cycle path from an SCC, if any
extractOneCycle :: Map.HashMap FilePath (Set.Set FilePath) -> Set.Set FilePath -> FilePath -> Maybe [FilePath]
extractOneCycle dirGraph allowed start = go start [start] (Set.singleton start)
 where
  go current path visited =
    let nexts = Set.filter (`Set.member` allowed) (Map.lookupDefault Set.empty current dirGraph)
     in case List.find (== start) (Set.toList nexts) of
          Just _ -> Just (path ++ [start])
          Nothing ->
            let unvisited = filter (`Set.notMember` visited) (Set.toList nexts)
             in foldr
                  (\n acc -> case acc of
                    Just p -> Just p
                    Nothing -> go n (path ++ [n]) (Set.insert n visited)
                  )
                  Nothing
                  unvisited

-- No cross-directory edge printing; we focus on sample module paths

-- | Enumerate simple module paths from dirA to dirB (bounded by depth and count)
findCrossModulePaths :: ModuleGraph -> FilePath -> FilePath -> Int -> Int -> [[Hir.ModuleText]]
findCrossModulePaths mg dirA dirB maxDepth maxPaths =
  let
      -- Normalize directory paths to avoid trailing-slash or ./ issues
      normalizeDir :: FilePath -> FilePath
      normalizeDir = FP.normalise . FP.dropTrailingPathSeparator

      dirA' = normalizeDir dirA
      dirB' = normalizeDir dirB

      nodes = mg.graphNodes
      dirOf m = maybe (moduleToDirectory m) (.moduleDirectory) (Map.lookup m nodes)
      sources = [ m | (m, n) <- Map.toList nodes, normalizeDir n.moduleDirectory == dirA' ]
      neighbors m = Set.toList (Map.lookupDefault Set.empty m mg.graphEdges)

      dfs :: Hir.ModuleText -> Int -> Set.Set Hir.ModuleText -> Int -> ([[Hir.ModuleText]], Int)
      dfs curr depth visited remaining
        | remaining <= 0 = ([], 0)
        | normalizeDir (dirOf curr) == dirB' = ([[curr]], remaining - 1)
        | depth <= 0 = ([], remaining)
        | otherwise =
            let nexts = filter (\n -> not (Set.member n visited)) (neighbors curr)
             in foldl
                  (\(accPaths, remCnt) n ->
                      if remCnt <= 0 then (accPaths, 0)
                      else
                        let (childPaths, remCnt') = dfs n (depth - 1) (Set.insert curr visited) remCnt
                            withCurr = map (curr :) childPaths
                         in (accPaths ++ withCurr, remCnt')
                  )
                  ([], remaining)
                  nexts

      collectFrom :: Int -> [Hir.ModuleText] -> ([[Hir.ModuleText]], Int)
      collectFrom remaining [] = ([], remaining)
      collectFrom remaining (s:ss)
        | remaining <= 0 = ([], 0)
        | otherwise =
            let (p1, r1) = dfs s maxDepth Set.empty remaining
                (p2, r2) = collectFrom r1 ss
             in (p1 ++ p2, r2)
      (paths, _) = collectFrom maxPaths sources
   in paths

-- | Determine whether a directory matches any of the target directories or prefixes
matchesTargets :: [FilePath] -> [FilePath] -> FilePath -> Bool
matchesTargets targetDirs targetPrefixes dir =
  List.any (== dir) targetDirs || List.any (`List.isPrefixOf` dir) targetPrefixes

-- Simplified: just build the graph; consumers will use crossDirectoryEdges
buildDirectoryCyclesContext :: [FilePath] -> IO (ModuleGraph, Map.HashMap FilePath (Set.Set FilePath))
buildDirectoryCyclesContext sourceDirs = do
  mg <- buildModuleGraphFromSourceDirs sourceDirs
  let dg = buildDirectoryGraph mg
  pure (mg, dg)

-- | Minimal stub to keep the executable working; customize as needed.
runDetectCycles :: IO ()
runDetectCycles = do
  -- Only print sample module paths between two targets
  let srcDirs = ["../mercury-web-backend/src"]
      dirA = "Mercury/Authorization"
      dirB = "PersistentModels"
  (mg, _dg) <- buildDirectoryCyclesContext srcDirs
  let maxDepth = 6
      maxPaths = 50
      pathsAB = findCrossModulePaths mg dirA dirB maxDepth maxPaths
      pathsBA = findCrossModulePaths mg dirB dirA maxDepth maxPaths
  putStrLn $ "\nSample module paths (" <> dirA <> " -> " <> dirB <> ") up to depth " <> show maxDepth <> ":"
  mapM_ (putStrLn . render . ModulePathView) pathsAB
  putStrLn $ "\nSample module paths (" <> dirB <> " -> " <> dirA <> ") up to depth " <> show maxDepth <> ":"
  mapM_ (putStrLn . render . ModulePathView) pathsBA

-- No CLI for now; script prints sample module paths between two hardcoded directories

-- | Convenience wrapper to run module renames end-to-end with a list of tuples.
-- It builds the program index from the given source roots, constructs
-- SourceEdits using `renameModule` for each rename, and then applies the
-- combined result with `applySourceEdit`.
--
-- Local variable to adjust:
--   - moduleRenames: list of (fromModule, toModule) tuples
runRenameModule :: IO ()
runRenameModule = do
  -- Adjust this list of (fromModule, toModule) tuples
  let moduleRenames = 
        [("Tasks.ChoiceRedshiftExport.Types.Core","Tasks.ChoiceRedshiftExport.Types")
        ]

  -- Discover modules under these source roots
  let sourceRoots = ["../mercury-web-backend/src", "../mercury-web-backend/test"]

  -- Build a ProgramIndex sufficient for renameModule
  modFileMap <- buildModuleFileMap sourceRoots
  -- Seed the index by parsing all known modules (best-effort)
  prgPairs <- forM (Map.toList modFileMap) $ \(modText, file) -> do
    prg <- parseProgramFromFile file
    pure (modText, prg)
  let programIndex = Map.fromList prgPairs

  -- Resolve base paths: old base is the current repo root; new base can be same or different
  oldBaseAbs <- Dir.getCurrentDirectory
  newBaseAbs <- Dir.makeAbsolute (fromMaybe "." (listToMaybe sourceRoots))

  -- Process each rename operation and combine SourceEdits
  sourceEdits <- forM moduleRenames $ \(oldModuleName, newModuleName) -> do
    let oldMod = parseModuleTextFromText oldModuleName
        newMod = parseModuleTextFromText newModuleName
    pure $ renameModule programIndex modFileMap oldBaseAbs newBaseAbs oldMod newMod

  -- Combine all SourceEdits and apply
  let combinedSourceEdit = mconcat sourceEdits
  applySourceEdit combinedSourceEdit

-- | Convenience wrapper mirroring runRenameModule's style to exercise pruneEdges
-- Adjust the local variables to your scenario, then run to apply the rename.
runPruneEdges :: IO ()
runPruneEdges = do
  -- Local variables
  let sourceRoots = ["../mercury-web-backend/src"]
      targetDir = "Mercury/FakeData"                 -- directory to pick the last module from
      newDir = "My.New.Module"                       -- new module prefix

      -- A sample path like:
      -- Mercury.FakeData.Populate.IntraMercuryTransfers
      --   -> Mercury.FakeData.Populate.MercuryAccount
      --   -> Mercury.FakeData.Constants
      pathModules =
        [ parseModuleTextFromText "Mercury.FakeData.Populate.IntraMercuryTransfers"
        , parseModuleTextFromText "Mercury.FakeData.Populate.MercuryAccount"
        , parseModuleTextFromText "Mercury.FakeData.Constants"
        ]
      pathView = ModulePathView pathModules

  se <- pruneEdges sourceRoots pathView targetDir newDir
  applySourceEdit se

-- | Given a discovered module path, choose the last module that resides
-- in the provided target directory, and rename it into a new module
-- prefix while preserving its leaf name. For example, if the path ends
-- with `Mercury.FakeData.Constants` and targetDir is `Mercury/FakeData`,
-- and newDir is `My.New.Module`, this will rename it to
-- `My.New.Module.Constants`.
--
-- This returns a SourceEdit; the caller may apply it with applySourceEdit.
pruneEdges :: [FilePath] -> ModulePathView -> FilePath -> String -> IO SourceEdit
pruneEdges sourceRoots (ModulePathView mods) targetDir newDir = do
  -- Build ProgramIndex and ModFileMap from the given source roots
  modFileMap <- buildModuleFileMap sourceRoots
  prgPairs <- forM (Map.toList modFileMap) $ \(modText, file) -> do
    prg <- parseProgramFromFile file
    pure (modText, prg)
  let programIndex = Map.fromList prgPairs

  -- Resolve bases: old base is current repo root; new base defaults to first source root
  oldBaseAbs <- Dir.getCurrentDirectory
  newBaseAbs <- Dir.makeAbsolute (fromMaybe "." (listToMaybe sourceRoots))

  let normalizeDir :: FilePath -> FilePath
      normalizeDir = FP.normalise . FP.dropTrailingPathSeparator

      targetDir' = normalizeDir targetDir

      -- Find the last module in the path whose directory matches targetDir
      pickCandidate :: Maybe Hir.ModuleText
      pickCandidate =
        let dirOf m = moduleToDirectory m
         in List.find (\m -> normalizeDir (dirOf m) == targetDir') (reverse mods)

  case pickCandidate of
    Nothing -> do
      -- Fallback: if nothing matches, act on the last module in the path
      let oldMod = fromMaybe (error "No modules in path") (listToMaybe (reverse mods))
      pure (renameIntoNewDir programIndex modFileMap oldBaseAbs newBaseAbs newDir oldMod)
    Just oldMod -> pure (renameIntoNewDir programIndex modFileMap oldBaseAbs newBaseAbs newDir oldMod)

 where
  -- Construct a SourceEdit to move a module to a new prefix, keeping its leaf name
  renameIntoNewDir :: ProgramIndex -> ModFileMap -> FilePath -> FilePath -> String -> Hir.ModuleText -> SourceEdit
  renameIntoNewDir prgIndex modMap oldBase newBase desiredNewDir mOld =
    let leafName :: String
        leafName =
          case T.splitOn (T.pack ".") mOld.text of
            [] -> T.unpack mOld.text
            xs -> T.unpack (fromMaybe "" (listToMaybe (reverse xs)))

        desiredNewDir' :: String
        desiredNewDir' = case reverse desiredNewDir of
          ('.':rest) -> reverse rest
          _ -> desiredNewDir

        newModNameStr :: T.Text
        newModNameStr = if null desiredNewDir' then T.pack leafName else T.pack desiredNewDir' <> T.pack "." <> T.pack leafName

        oldMod = mOld
        newMod = parseModuleTextFromText newModNameStr
     in renameModule prgIndex modMap oldBase newBase oldMod newMod

-- | Rename all modules that match a given prefix to a new prefix
-- This function finds all modules in the program index that start with the old prefix
-- and renames them to use the new prefix instead, preserving the suffix.
--
-- For example, if oldPrefix is "Mercury.FakeData" and newPrefix is "My.New.Module",
-- then "Mercury.FakeData.Core" becomes "My.New.Module.Core"
--
-- This returns a SourceEdit; the caller may apply it with applySourceEdit.
renameModulePrefix :: ProgramIndex -> ModFileMap -> FilePath -> FilePath -> String -> String -> SourceEdit
renameModulePrefix prgIndex modMap oldBaseAbs newBaseAbs oldPrefix newPrefix =
  let
      -- Normalize prefixes to handle trailing dots
      normalizePrefix :: String -> String
      normalizePrefix = reverse . dropWhile (== '.') . reverse

      oldPrefix' = normalizePrefix oldPrefix
      newPrefix' = normalizePrefix newPrefix

      -- Find all modules that start with the old prefix (including exact matches)
      matchingModules :: [Hir.ModuleText]
      matchingModules = 
        let oldPrefixText = T.pack oldPrefix'
         in [ modText | modText <- Map.keys prgIndex
             , T.isPrefixOf oldPrefixText modText.text
             ]

      -- For each matching module, create a rename operation
      renameOperations :: [SourceEdit]
      renameOperations = 
        [ let
              -- Extract the suffix (everything after the old prefix)
              suffix = T.drop (T.length (T.pack oldPrefix')) modText.text
              -- Remove leading dot if present (handle empty suffix case)
              suffix' = if T.null suffix 
                        then suffix  -- Empty suffix, no leading dot to remove
                        else if maybe False (== '.') (listToMaybe (T.unpack suffix)) then T.tail suffix else suffix
              -- Construct new module name
              newModNameStr = if T.null suffix' 
                             then T.pack newPrefix'
                             else T.pack newPrefix' <> T.pack "." <> suffix'
              newMod = parseModuleTextFromText newModNameStr
           in renameModule prgIndex modMap oldBaseAbs newBaseAbs modText newMod
        | modText <- matchingModules
        ]

   in mconcat renameOperations

-- | Convenience wrapper to rename all modules matching a prefix to a new prefix
-- Adjust the local variables to your scenario, then run to apply the renames.
--
-- Local variables to adjust:
--   - sourceRoots: list of source directories to scan
--   - oldPrefix: the prefix to match (e.g., "Mercury.FakeData")
--   - newPrefix: the new prefix to use (e.g., "My.New.Module")
runRenameModulePrefix :: IO ()
runRenameModulePrefix = do
  -- Adjust these local variables
  let sourceRoots = ["../mercury-web-backend/src", "../mercury-web-backend/test"]
      oldPrefix = "Mobile.TwoFactorAuth.AuthCode"
      newPrefix = "Mobile.TwoFactorAuth.Core.AuthCode"

  -- Build a ProgramIndex sufficient for renameModule
  modFileMap <- buildModuleFileMap sourceRoots
  -- Seed the index by parsing all known modules (best-effort)
  prgPairs <- forM (Map.toList modFileMap) $ \(modText, file) -> do
    prg <- parseProgramFromFile file
    pure (modText, prg)
  let programIndex = Map.fromList prgPairs

  -- Resolve base paths: old base is the current repo root; new base can be same or different
  oldBaseAbs <- Dir.getCurrentDirectory
  newBaseAbs <- Dir.makeAbsolute (fromMaybe "." (listToMaybe sourceRoots))

  -- Create SourceEdit for prefix rename and apply
  let sourceEdit = renameModulePrefix programIndex modFileMap oldBaseAbs newBaseAbs oldPrefix newPrefix
  applySourceEdit sourceEdit

