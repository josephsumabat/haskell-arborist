{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildGraph.Directory where

import Arborist.Files (ModFileMap, buildModuleFileMap)
import Arborist.GlobImports (globImportModules)
import Arborist.ProgramIndex (ProgramIndex, gatherScopeDeps, gatherTransitiveDeps, getPrgs)
import BuildGraph.Directory.TH (targetKeyTagModifier)
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Aeson (SumEncoding (..))
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.ByteString qualified as BS
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl')
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import HaskellAnalyzer (parsePrg)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import System.Directory (makeAbsolute)
import System.FilePath
  ( dropTrailingPathSeparator
  , isAbsolute
  , makeRelative
  , normalise
  , splitDirectories
  , takeDirectory
  , (</>)
  )
import Optics ((.~))
import Optics.TH (makeFieldLabelsNoPrefix)
import qualified Data.HashMap.Lazy as Map
import Arborist.Debug.Trace
import Hir.Parse

-- | Error cases encountered while attempting to build a maximal acyclic graph
data ModuleCycleEdge = ModuleCycleEdge
  { sourceModule :: Hir.ModuleText
  , importedModule :: Hir.ModuleText
  }
  deriving (Eq, Show)

unknownModule :: Hir.ModuleText
unknownModule = Hir.ModuleText {parts = "Unknown" :| [], text = "Unknown"}

data BuildGraphError
  = ModuleCycleDetected (NonEmpty ModuleCycleEdge)
  deriving (Eq, Show)

-- | Canonical representation of a directory (split on module components)
newtype DirName = DirName {dirText :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show DirName where
  show (DirName text) = T.unpack text

-- | Information about the root directory used for relative path calculations
data RootDirectory = RootDirectory
  { rootPath :: FilePath
  , rootLabel :: Text
  }
  deriving (Eq, Show)

-- | Identifier for targets in the directory graph
data TargetKey
  = DirectoryTarget DirName
  | RecursiveDirectoryTarget DirName
  | ModuleTarget Hir.ModuleText
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Finalized target representation exposed to callers
data Target = Target
  { key :: TargetKey
  , dir :: DirName
  , modules :: Set Hir.ModuleText
  , dependsOn :: Set Hir.ModuleText
  }
  deriving (Eq, Show)

data MaxDirTargetGraph = MaxDirTargetGraph
  { targets :: HashMap TargetKey Target
  , moduleToTarget :: HashMap Hir.ModuleText TargetKey
  , rootInfo :: RootDirectory
  }
  deriving (Eq, Show)

-- | Serializable representation of the target graph
data BuildGraphOutput = BuildGraphOutput
  { targets :: [TargetOutput]
  , moduleToTarget :: HashMap Text TargetKeyOutput
  }
  deriving (Eq, Show, Generic)

data TargetOutput = TargetOutput
  { key :: TargetKeyOutput
  , directory :: Text
  , modules :: [Text]
  , dependsOn :: [Text]
  }
  deriving (Eq, Show, Generic)

data TargetKeyOutput
  = DirectoryTargetOutput Text
  | RecursiveDirectoryTargetOutput Text
  | ModuleTargetOutput Text
  deriving (Eq, Show, Generic)

$(deriveJSON
    defaultOptions
      { sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "name"}
      , constructorTagModifier = targetKeyTagModifier
      }
    ''TargetKeyOutput
 )
$(deriveJSON defaultOptions ''TargetOutput)
$(deriveJSON defaultOptions ''BuildGraphOutput)

-- Internal representations ----------------------------------------------------

data ModuleLocation
  = LocalModule DirName
  | ExternalModule
  deriving (Eq, Show)

data ModuleInfo = ModuleInfo
  { name :: Hir.ModuleText
  , location :: ModuleLocation
  , imports :: Set Hir.ModuleText
  }
  deriving (Eq, Show)

data TargetState = TargetState
  { key :: TargetKey
  , dir :: DirName
  , modules :: Set Hir.ModuleText
  }

data BuildState = BuildState
  { moduleInfos :: HashMap Hir.ModuleText ModuleInfo
  , moduleToTarget :: HashMap Hir.ModuleText TargetKey
  , targets :: HashMap TargetKey TargetState
  , rootInfo :: RootDirectory
  }

$(makeFieldLabelsNoPrefix ''BuildState)

-- Public API -----------------------------------------------------------------

buildMaxDirTargetGraph :: RootDirectory -> ProgramIndex -> ModFileMap -> Either BuildGraphError MaxDirTargetGraph
buildMaxDirTargetGraph rootInfo programIndex fullModFileMap =
  buildMaxDirTargetGraphWithRecursiveTargets rootInfo programIndex fullModFileMap Set.empty

buildMaxDirTargetGraphWithRecursiveTargets :: RootDirectory -> ProgramIndex -> ModFileMap -> Set DirName -> Either BuildGraphError MaxDirTargetGraph
buildMaxDirTargetGraphWithRecursiveTargets rootInfo programIndex fullModFileMap recursiveTargetDirs = do
  buildState <- resolveDirectoryTargets (initialBuildState rootInfo programIndex fullModFileMap recursiveTargetDirs)
  pure (finalizeGraph buildState)

buildGraphFromDirectories :: FilePath -> [FilePath] -> IO (Either BuildGraphError MaxDirTargetGraph)
buildGraphFromDirectories rootDir sourceDirs =
  buildGraphFromDirectoriesWithRecursiveTargets rootDir sourceDirs []

buildGraphFromDirectoriesWithRecursiveTargets :: FilePath -> [FilePath] -> [FilePath] -> IO (Either BuildGraphError MaxDirTargetGraph)
buildGraphFromDirectoriesWithRecursiveTargets rootDir sourceDirs recursiveTargetDirs = do
  rootInfo <- normalizeRootDirectory rootDir
  rootedDirs <- mapM (resolveSourceDir rootInfo) sourceDirs
  localModFileMap <- buildModuleFileMap rootedDirs
  fullModFileMap <- buildModuleFileMap [rootInfo.rootPath]
  --programIndex <- loadProgramIndex localModFileMap fullModFileMap
  programIndex <- loadSimpleProgramIndex (Map.elems localModFileMap) fullModFileMap
  --programIndex <- loadAllPrograms [rootDir]
  recursiveDirs <- mapM (resolveRecursiveTargetDir rootInfo) recursiveTargetDirs
  let recursiveDirSet = Set.fromList recursiveDirs
  pure (buildMaxDirTargetGraphWithRecursiveTargets rootInfo programIndex fullModFileMap recursiveDirSet)

loadSimpleProgramIndex :: [FilePath] -> ModFileMap -> IO ProgramIndex
loadSimpleProgramIndex files fullModFileMap =
  foldM
    (\acc file -> do
        program <- parseProgramFromFile file
        gatherTransitiveDeps acc program fullModFileMap
    )
    HM.empty
    files

loadProgramIndex :: ModFileMap -> ModFileMap -> IO ProgramIndex
loadProgramIndex files fullModFileMap =
  go HM.empty Set.empty (HM.elems files)
 where
  go :: ProgramIndex -> Set FilePath -> [FilePath] -> IO ProgramIndex
  go acc visitedDirs pendingFiles = do
    acc' <- loadFiles acc pendingFiles
    let moduleDirs = moduleDirectories acc'
        newDirs = Set.difference moduleDirs visitedDirs
    if Set.null newDirs
      then pure acc'
      else do
        let nextVisited = Set.union visitedDirs newDirs
            siblingFiles = siblingFilesForDirs acc' newDirs
        go acc' nextVisited siblingFiles

  loadFiles :: ProgramIndex -> [FilePath] -> IO ProgramIndex
  loadFiles acc [] = pure acc
  loadFiles acc filePaths =
    foldM loadFile acc (Set.toList (Set.fromList filePaths))

  loadFile :: ProgramIndex -> FilePath -> IO ProgramIndex
  loadFile acc file =
    case HM.lookup file fileToModule of
      Just moduleText | HM.member moduleText acc -> pure acc
      _ -> do
        program <- parseProgramFromFile file
        let accWithSelf =
              maybe
                Map.empty
                (\moduleText -> HM.insert moduleText program acc)
                program.mod
        gatherScopeDeps accWithSelf program fullModFileMap Nothing

  fileToModule :: HashMap FilePath Hir.ModuleText
  fileToModule = HM.fromList [(file, moduleText) | (moduleText, file) <- HM.toList fullModFileMap]

  moduleDirectories :: ProgramIndex -> Set FilePath
  moduleDirectories index =
    Set.fromList (map takeDirectory (mapMaybe (`HM.lookup` fullModFileMap) (HM.keys index)))

  siblingFilesForDirs :: ProgramIndex -> Set FilePath -> [FilePath]
  siblingFilesForDirs index dirs =
    Set.toList . Set.fromList $
      [ file
      | (moduleText, file) <- HM.toList fullModFileMap
      , takeDirectory file `Set.member` dirs
      , not (HM.member moduleText index)
      ]

loadAllPrograms :: [FilePath] -> IO ProgramIndex
loadAllPrograms dirs = do
  absDirs <- mapM makeAbsolute dirs
  fullModFileMap <- buildModuleFileMap absDirs
  snd <$> getPrgs Map.empty (Map.toList fullModFileMap)

-- Construction ----------------------------------------------------------------

initialBuildState :: RootDirectory -> ProgramIndex -> ModFileMap -> Set DirName -> BuildState
initialBuildState rootInfo programIndex fullModFileMap recursiveTargetDirs =
  let moduleInfos =
        HM.fromList
          [ (moduleName, toInfo moduleName program)
          | (moduleName, program) <- HM.toList programIndex
          ]
      assignments =
        mapMaybe
          (\(moduleName, info) -> do
              (targetKey, targetDir) <- moduleTargetAssignment info
              pure (moduleName, targetKey, targetDir)
          )
          (HM.toList moduleInfos)

      moduleToTarget =
        HM.fromList
          [ (moduleName, targetKey)
          | (moduleName, targetKey, _) <- assignments
          ]

      targets =
        HM.fromListWith mergeTargets
          [ ( targetKey
            , TargetState
                { key = targetKey
                , dir = targetDir
                , modules = Set.singleton moduleName
                }
            )
          | (moduleName, targetKey, targetDir) <- assignments
          ]
   in  BuildState {moduleInfos, moduleToTarget, targets, rootInfo}
 where
  toInfo :: Hir.ModuleText -> Hir.Read.Program -> ModuleInfo
  toInfo moduleName program = traceIdWhen (moduleName == parseModuleTextFromText "Mercury.Network.Bugsnag.Init") $
    case HM.lookup moduleName fullModFileMap of
      Just filePath ->
        localInfo filePath
      Nothing -> externalInfo

   where
    localInfo filePath =
      ModuleInfo
        { name = moduleName
        , location = LocalModule (moduleDirectory rootInfo filePath)
        , imports = moduleImports
        }
    externalInfo =
      ModuleInfo
        { name = moduleName
        , location = ExternalModule
        , imports = moduleImports
        }

    moduleImports =
      let directImports = (.mod) <$> program.imports
          globImports = globImportModules fullModFileMap moduleName program
       in Set.fromList (directImports ++ globImports)

  moduleTargetAssignment :: ModuleInfo -> Maybe (TargetKey, DirName)
  moduleTargetAssignment info =
    case info.location of
      LocalModule moduleDir ->
        case findRecursiveTargetDir recursiveTargetDirs moduleDir of
          Just recursiveDir -> Just (RecursiveDirectoryTarget recursiveDir, recursiveDir)
          Nothing -> Just (DirectoryTarget moduleDir, moduleDir)
      ExternalModule -> Nothing

  mergeTargets :: TargetState -> TargetState -> TargetState
  mergeTargets left right =
    TargetState
      { key = left.key
      , dir = left.dir
      , modules = Set.union left.modules right.modules
      }

  findRecursiveTargetDir :: Set DirName -> DirName -> Maybe DirName
  findRecursiveTargetDir dirs moduleDir =
    case filter (`dirIsAncestorOf` moduleDir) (Set.toList dirs) of
      [] -> Nothing
      matches -> Just (List.maximumBy (comparing dirDepth) matches)

  dirIsAncestorOf :: DirName -> DirName -> Bool
  dirIsAncestorOf ancestor child =
    let ancestorSegments = dirNameSegments ancestor
        childSegments = dirNameSegments child
     in List.isPrefixOf ancestorSegments childSegments

  dirDepth :: DirName -> Int
  dirDepth = length . dirNameSegments

  dirNameSegments :: DirName -> [Text]
  dirNameSegments (DirName text) = filter (not . T.null) (T.splitOn "." text)

moduleDirectory :: RootDirectory -> FilePath -> DirName
moduleDirectory rootInfo filePath =
  let dirPath = normalise (takeDirectory filePath)
   in DirName (directoryText rootInfo dirPath)


normalizeRootDirectory :: FilePath -> IO RootDirectory
normalizeRootDirectory path = do
  absRoot <- makeAbsolute path
  let normalized = dropTrailingPathSeparator (normalise absRoot)
      label = "."
  pure RootDirectory {rootPath = normalized, rootLabel = label}

resolveSourceDir :: RootDirectory -> FilePath -> IO FilePath
resolveSourceDir root dir = do
  absInput <- makeAbsolute dir
  let normalizedAbs = normalise absInput
      relativeToRoot = normalise (makeRelative root.rootPath normalizedAbs)
  if isOutsideRoot relativeToRoot
    then
      if isAbsolute dir
        then pure normalizedAbs
        else pure (normalise (root.rootPath </> dir))
    else pure normalizedAbs
 where
 isOutsideRoot rel = ".." `List.isPrefixOf` rel

resolveRecursiveTargetDir :: RootDirectory -> FilePath -> IO DirName
resolveRecursiveTargetDir root dir = do
  resolvedDir <- resolveSourceDir root dir
  let normalized = dropTrailingPathSeparator (normalise resolvedDir)
  pure (DirName (directoryText root normalized))

-- Cycle resolution ------------------------------------------------------------

resolveDirectoryTargets :: BuildState -> Either BuildGraphError BuildState
resolveDirectoryTargets = go
 where
  go state =
    let (deps, edgeMap) = computeTargetDependencies state
        nodes = buildGraphNodes state deps
        cycles = [vertices | CyclicSCC vertices <- stronglyConnComp nodes]
     in case cycles of
          [] -> Right state
          _ ->
            let (state', changed) = foldl' (splitCycle edgeMap) (state, False) cycles
             in if changed
                  then go state'
                  else Left (ModuleCycleDetected (pickCycleEdges deps edgeMap state cycles))

splitCycle :: HashMap (TargetKey, TargetKey) ModuleCycleEdge -> (BuildState, Bool) -> [TargetKey] -> (BuildState, Bool)
splitCycle edgeMap (state, changed) vertices =
  let cycleSet = Set.fromList vertices
   in foldl'
        (splitTarget edgeMap cycleSet)
        (state, changed)
        vertices

splitTarget :: HashMap (TargetKey, TargetKey) ModuleCycleEdge -> Set TargetKey -> (BuildState, Bool) -> TargetKey -> (BuildState, Bool)
splitTarget _edgeMap cycleSet (state, changed) key =
  case HM.lookup key state.targets of
    Nothing -> (state, changed)
    Just targetState ->
      case key of
        ModuleTarget _ -> (state, changed)
        DirectoryTarget _ -> splitDirectoryLike targetState
        RecursiveDirectoryTarget _ -> splitDirectoryLike targetState
 where
  splitDirectoryLike targetState =
    let offenders = offendingModules state cycleSet key targetState
     in if null offenders
          then (state, changed)
          else
            let state' = foldl' (\acc moduleName -> splitModule key moduleName acc) state offenders
             in (state', True)

offendingModules :: BuildState -> Set TargetKey -> TargetKey -> TargetState -> [Hir.ModuleText]
offendingModules state cycleSet key targetState =
  filter (moduleCausesCycle state cycleSet key) (Set.toList targetState.modules)

moduleCausesCycle :: BuildState -> Set TargetKey -> TargetKey -> Hir.ModuleText -> Bool
moduleCausesCycle state cycleSet key moduleName =
  case HM.lookup moduleName state.moduleInfos of
    Nothing -> False
    Just info ->
      any
        (\dep -> case HM.lookup dep state.moduleToTarget of
          Just depKey -> depKey /= key && Set.member depKey cycleSet
          Nothing -> False
        )
        (Set.toList info.imports)

splitModule :: TargetKey -> Hir.ModuleText -> BuildState -> BuildState
splitModule parentKey moduleName state =
  case HM.lookup parentKey state.targets of
    Nothing -> state
    Just parent ->
      let remainingModules = Set.delete moduleName parent.modules
          targetsWithoutModule =
            if Set.null remainingModules
              then HM.delete parentKey state.targets
              else HM.insert parentKey (withModules parent remainingModules) state.targets

       in case HM.lookup moduleName state.moduleInfos of
            Nothing -> state
            Just moduleInfo ->
              let newKey = ModuleTarget moduleName
                  moduleDir =
                    case parent.key of
                      RecursiveDirectoryTarget parentDir -> parentDir
                      _ ->
                        case moduleInfo.location of
                          LocalModule dir -> dir
                          ExternalModule -> parent.dir
                  newTarget =
                    TargetState
                      { key = newKey
                      , dir = moduleDir
                      , modules = Set.singleton moduleName
                      }
                  targetsWithNew = HM.insert newKey newTarget targetsWithoutModule
                  moduleToTarget = HM.insert moduleName newKey state.moduleToTarget
               in state
                    & #targets .~ targetsWithNew
                    & #moduleToTarget .~ moduleToTarget

withModules :: TargetState -> Set Hir.ModuleText -> TargetState
withModules targetState newModules =
  TargetState
    { key = targetState.key
    , dir = targetState.dir
    , modules = newModules
    }

pickCycleEdges :: HashMap TargetKey (Set TargetKey) -> HashMap (TargetKey, TargetKey) ModuleCycleEdge -> BuildState -> [[TargetKey]] -> NonEmpty ModuleCycleEdge
pickCycleEdges _ _ _ [] = ModuleCycleEdge unknownModule unknownModule :| []
pickCycleEdges deps edgeMap state (cycleVertices : _) =
  case NE.nonEmpty explicitEdges of
    Just edges -> edges
    Nothing -> ModuleCycleEdge (pickModule fallbackTarget) (pickModule fallbackTarget) :| []
 where
  cycleSet = Set.fromList cycleVertices

  explicitEdges =
    [ fromMaybe (fallbackEdge fromTarget toTarget) (lookupEdge fromTarget toTarget)
    | fromTarget <- cycleVertices
    , toTarget <- Set.toList (HM.lookupDefault Set.empty fromTarget deps)
    , Set.member toTarget cycleSet
    ]

  lookupEdge fromTarget toTarget =
    HM.lookup (fromTarget, toTarget) edgeMap <|> findEdge fromTarget toTarget

  findEdge fromTarget toTarget = do
    targetState <- HM.lookup fromTarget state.targets
    let modules = Set.toList targetState.modules
        candidateEdges =
          [ ModuleCycleEdge moduleName depName
          | moduleName <- modules
          , info <- maybeToList (HM.lookup moduleName state.moduleInfos)
          , depName <- Set.toList info.imports
          , HM.lookup depName state.moduleToTarget == Just toTarget
          ]
    listToMaybe candidateEdges

  fallbackEdge fromTarget toTarget =
    ModuleCycleEdge (pickModule fromTarget) (pickModule toTarget)

  pickModule targetKey =
    case targetModules state targetKey of
      moduleName : _ -> moduleName
      [] -> unknownModule

  fallbackTarget = case cycleVertices of
    vertex : _ -> vertex
    [] -> DirectoryTarget (DirName (T.pack "Unknown"))


targetModules :: BuildState -> TargetKey -> [Hir.ModuleText]
targetModules state key =
  case HM.lookup key state.targets of
    Just targetState -> Set.toList targetState.modules
    Nothing -> []

-- Dependency analysis ---------------------------------------------------------

computeTargetDependencies :: BuildState -> (HashMap TargetKey (Set TargetKey), HashMap (TargetKey, TargetKey) ModuleCycleEdge)
computeTargetDependencies state =
  HM.foldlWithKey'
    (\(depMap, edgeMap) key targetState ->
        let (depsForKey, edgesForKey) = collectDeps state key targetState
            depMap' = HM.insert key depsForKey depMap
            edgeMap' = HM.union edgeMap (HM.mapKeys (key,) edgesForKey)
         in (depMap', edgeMap')
    )
    (HM.empty, HM.empty)
    state.targets

collectDeps :: BuildState -> TargetKey -> TargetState -> (Set TargetKey, HashMap TargetKey ModuleCycleEdge)
collectDeps state key targetState =
  foldl'
    (\(depSet, edgeMap) moduleName ->
        case HM.lookup moduleName state.moduleInfos of
          Nothing -> (depSet, edgeMap)
          Just info ->
            foldl'
              (\(accSet, accMap) depName ->
                  case HM.lookup depName state.moduleToTarget of
                    Just depKey | depKey /= key ->
                      ( Set.insert depKey accSet
                      , HM.insert depKey (ModuleCycleEdge moduleName depName) accMap
                      )
                    _ -> (accSet, accMap)
              )
              (depSet, edgeMap)
              (Set.toList info.imports)
    )
    (Set.empty, HM.empty)
    (Set.toList targetState.modules)

computeTargetModuleDependencies :: BuildState -> HashMap TargetKey (Set Hir.ModuleText)
computeTargetModuleDependencies state =
  HM.mapWithKey (collectModuleDeps state) state.targets

collectModuleDeps :: BuildState -> TargetKey -> TargetState -> Set Hir.ModuleText
collectModuleDeps state key targetState =
  Set.unions (moduleDeps <$> Set.toList targetState.modules)
 where
  moduleDeps moduleName =
    case HM.lookup moduleName state.moduleInfos of
      Nothing -> Set.empty
      Just info ->
        Set.fromList
          [ depName
          | depName <- Set.toList info.imports
          , HM.lookup depName state.moduleToTarget /= Just key
          ]

buildGraphNodes :: BuildState -> HashMap TargetKey (Set TargetKey) -> [(TargetKey, TargetKey, [TargetKey])]
buildGraphNodes state deps =
  [ (key, key, Set.toList (HM.lookupDefault Set.empty key deps))
  | key <- HM.keys state.targets
  ]

-- Finalization ----------------------------------------------------------------

finalizeGraph :: BuildState -> MaxDirTargetGraph
finalizeGraph state =
  let moduleDeps = computeTargetModuleDependencies state
      materialize targetState =
        Target
          { key = targetState.key
          , dir = targetState.dir
          , modules = targetState.modules
          , dependsOn = HM.lookupDefault Set.empty targetState.key moduleDeps
          }
      targets = HM.map materialize state.targets
      moduleAssignments = state.moduleToTarget
   in MaxDirTargetGraph {targets, moduleToTarget = moduleAssignments, rootInfo = state.rootInfo}

graphToOutput :: MaxDirTargetGraph -> BuildGraphOutput
graphToOutput graph =
  BuildGraphOutput
    { targets = map targetToOutput (HM.elems graph.targets)
    , moduleToTarget = moduleAssignments
    }
 where
  targetToOutput :: Target -> TargetOutput
  targetToOutput target =
    TargetOutput
      { key = keyOutput target.key
      , directory = dirNameToText target.dir
      , modules = map qualifiedModuleName (Set.toList target.modules)
      , dependsOn = map qualifiedModuleName (Set.toList target.dependsOn)
      }

  qualifiedModuleName :: Hir.ModuleText -> Text
  qualifiedModuleName moduleName = moduleName.text

  moduleAssignments :: HashMap Text TargetKeyOutput
  moduleAssignments =
    HM.fromList
      [ (qualifiedModuleName moduleName, keyOutput targetKey)
      | (moduleName, targetKey) <- HM.toList graph.moduleToTarget
      ]

  keyOutput :: TargetKey -> TargetKeyOutput
  keyOutput key =
    case key of
      DirectoryTarget dirName -> DirectoryTargetOutput (dirNameToText dirName)
      RecursiveDirectoryTarget dirName -> RecursiveDirectoryTargetOutput (dirNameToText dirName)
      ModuleTarget moduleName -> ModuleTargetOutput moduleName.text

renderBuildGraphError :: BuildGraphError -> String
renderBuildGraphError (ModuleCycleDetected edges) =
  let header = "Unable to build acyclic target graph; cycle detected:\n"
      body = unlines (map formatEdge (NE.toList edges))
   in header <> body
 where
  formatEdge ModuleCycleEdge {sourceModule, importedModule} =
    T.unpack (moduleTextToText sourceModule)
      <> " imports "
      <> T.unpack (moduleTextToText importedModule)

dirNameToText :: DirName -> Text
dirNameToText (DirName text) = text

moduleTextToText :: Hir.ModuleText -> Text
moduleTextToText = (.text)

parseProgramFromFile :: FilePath -> IO Hir.Read.Program
parseProgramFromFile file = do
  bytes <- BS.readFile file
  let (_src, prg) = parsePrg (TE.decodeUtf8 bytes)
  pure prg

directoryText :: RootDirectory -> FilePath -> Text
directoryText root dirPath =
  let relativeRaw = makeRelative root.rootPath dirPath
      relative = normalise relativeRaw
   in if relative == "." || null relative
        then root.rootLabel
        else if isOutsideRoot relative
          then T.pack dirPath
          else pathToDot relative
 where
  isOutsideRoot rel = ".." `List.isPrefixOf` rel

pathToDot :: FilePath -> Text
pathToDot fp =
  let segments = filter (not . null) (splitDirectories fp)
   in case segments of
        [] -> ""
        _ -> T.intercalate "." (map T.pack segments)
