{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildGraph.Directory where

import Arborist.Files (ModFileMap, buildModuleFileMap)
import Arborist.GlobImports (globImportModules)
import Arborist.PersistentModels qualified as PersistentModels
import Arborist.ProgramIndex (ProgramIndex, gatherScopeDeps, gatherTransitiveDeps, getPrgs)
import BuildGraph.Directory.TH (targetKeyTagModifier)
import BuildGraph.ModuleTargetOverrides (
  ModuleTargetOverrides (..),
  TargetOverride (..),
  emptyTargetOverrides,
 )
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM)
import Data.Aeson (SumEncoding (..))
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.HashMap.Lazy qualified as Map
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (find, foldl', sortOn)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import HaskellAnalyzer (parsePrg)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import Optics ((.~))
import Optics.TH (makeFieldLabelsNoPrefix)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (
  dropTrailingPathSeparator,
  isAbsolute,
  isPathSeparator,
  makeRelative,
  normalise,
  takeDirectory,
  (</>),
 )

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
  | ModuleAssignedMultipleTargets Hir.ModuleText (NonEmpty ModuleTargetAssignment)
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
  , sourcePrefix :: Maybe Text
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
  , persistentSrcDeps :: [(Hir.ModuleText, (Text, [Text]))]
  }
  deriving (Eq, Show)

data ModuleTargetAssignment
  = AssignedTarget TargetKey
  | PreassignedTarget TargetOverride
  deriving (Eq, Show)

data MaxDirTargetGraph = MaxDirTargetGraph
  { targets :: HashMap TargetKey Target
  , moduleToTarget :: HashMap Hir.ModuleText ModuleTargetAssignment
  , rootInfo :: RootDirectory
  }
  deriving (Eq, Show)

-- | Serializable representation of the target graph
data BuildGraphOutput = BuildGraphOutput
  { targets :: [TargetOutput]
  , moduleToTarget :: HashMap Text Text
  }
  deriving (Eq, Show, Generic)

data TargetOutput = TargetOutput
  { key :: TargetKeyOutput
  , targetName :: Text
  , targetKey :: Text
  , directory :: Text
  , modules :: [Text]
  , dependsOn :: [Text]
  , srcDeps :: [(Text, [Text])]
  }
  deriving (Eq, Show, Generic)

data TargetKeyOutput
  = DirectoryTargetOutput Text
  | RecursiveDirectoryTargetOutput Text
  | ModuleTargetOutput Text
  | ExternalTargetOutput Text
  deriving (Eq, Show, Generic)

$( deriveJSON
    defaultOptions
      { sumEncoding = TaggedObject {tagFieldName = "type", contentsFieldName = "name"}
      , constructorTagModifier = targetKeyTagModifier
      }
    ''TargetKeyOutput
 )
$(deriveJSON defaultOptions ''TargetOutput)
$(deriveJSON defaultOptions ''BuildGraphOutput)

dirNameToText :: DirName -> Text
dirNameToText (DirName text) = text

moduleTextToText :: Hir.ModuleText -> Text
moduleTextToText = (.text)

persistentModelsRoot :: Text
persistentModelsRoot = "config/modelsFiles"

stripLeadingSlashes :: Text -> Text
stripLeadingSlashes = T.dropWhile (== '/')


trimTrailingSlash :: Text -> Text
trimTrailingSlash txt
  | T.null txt = txt
  | otherwise = T.dropWhileEnd (== '/') txt

targetNameFromKeyOutput :: TargetKeyOutput -> Text
targetNameFromKeyOutput key =
  case key of
    DirectoryTargetOutput dir -> dir
    RecursiveDirectoryTargetOutput dir -> dir
    ModuleTargetOutput name -> name
    ExternalTargetOutput name -> name

renderName :: RootDirectory -> DirName -> Hir.ModuleText -> Text
renderName rootInfo dir moduleName =
  "//" <> renderDirectoryPath rootInfo dir <> ":" <> moduleTargetSlug (moduleTextToText moduleName)

renderDirectoryPath :: RootDirectory -> DirName -> Text
renderDirectoryPath _ dir = normalizeDirText (dirNameToText dir)

moduleTargetSlug :: Text -> Text
moduleTargetSlug = T.toLower . T.replace "." "_"

normalizeDirText :: Text -> Text
normalizeDirText text
  | cleaned == "." = ""
  | otherwise = stripLeadingSlashes cleaned
 where
  cleaned = trimTrailingSlash text

-- Internal representations ----------------------------------------------------

data ModuleLocation
  = LocalModule DirName
  | ExternalModule
  deriving (Eq, Show)

data ModuleInfo = ModuleInfo
  { name :: Hir.ModuleText
  , location :: ModuleLocation
  , imports :: Set Hir.ModuleText
  , sourceFile :: Maybe Text
  , persistentModels :: [Text]
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
  , preassignedTargets :: HashMap Hir.ModuleText TargetOverride
  }

$(makeFieldLabelsNoPrefix ''BuildState)

-- Public API -----------------------------------------------------------------

buildMaxDirTargetGraph :: RootDirectory -> DirName -> Set DirName -> ProgramIndex -> ModFileMap -> Maybe ModuleTargetOverrides -> Either BuildGraphError MaxDirTargetGraph
buildMaxDirTargetGraph rootInfo rootTargetDir buckDirs programIndex fullModFileMap overrides =
  buildMaxDirTargetGraphWithRecursiveTargets rootInfo rootTargetDir buckDirs programIndex fullModFileMap Set.empty overrides

buildMaxDirTargetGraphWithRecursiveTargets :: RootDirectory -> DirName -> Set DirName -> ProgramIndex -> ModFileMap -> Set DirName -> Maybe ModuleTargetOverrides -> Either BuildGraphError MaxDirTargetGraph
buildMaxDirTargetGraphWithRecursiveTargets rootInfo rootTargetDir buckDirs programIndex fullModFileMap recursiveTargetDirs overrides = do
  let hasOverrideMap = isJust overrides
      activeOverrides = fromMaybe emptyTargetOverrides overrides
  buildState <- resolveDirectoryTargets (initialBuildState rootInfo rootTargetDir buckDirs programIndex fullModFileMap recursiveTargetDirs hasOverrideMap activeOverrides)
  validateTargetAssignments buildState
  pure (finalizeGraph buildState)

buildGraphFromDirectories :: FilePath -> FilePath -> [FilePath] -> Maybe ModuleTargetOverrides -> IO (Either BuildGraphError MaxDirTargetGraph)
buildGraphFromDirectories srcRoot rootBuckDir sourceDirs overrides =
  buildGraphFromDirectoriesWithRecursiveTargets srcRoot rootBuckDir sourceDirs [] overrides

buildGraphFromDirectoriesWithRecursiveTargets :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> Maybe ModuleTargetOverrides -> IO (Either BuildGraphError MaxDirTargetGraph)
buildGraphFromDirectoriesWithRecursiveTargets srcRoot rootBuckDir targetDirs recursiveTargetDirs overrides = do
  rootInfo <- normalizeRootDirectory srcRoot rootBuckDir
  buckDirs <- collectBuckDirectories rootInfo
  rootedDirs <- mapM (resolveSourceDir rootInfo) targetDirs
  localModFileMap <- buildModuleFileMap rootedDirs
  fullModFileMap <- buildModuleFileMap [rootInfo.rootPath]
  programIndex <-
    case targetDirs of
      [] -> loadSimpleProgramIndex (Map.elems localModFileMap) fullModFileMap
      _ -> loadAllPrograms [srcRoot]
  -- programIndex <- loadProgramIndex localModFileMap fullModFileMap
  -- programIndex <- loadAllPrograms [rootDir]
  recursiveDirs <- mapM (resolveRecursiveTargetDir rootInfo) recursiveTargetDirs
  rootTargetDir <- resolveRootTargetDir rootInfo rootBuckDir
  let recursiveDirSet = Set.fromList recursiveDirs

  pure (buildMaxDirTargetGraphWithRecursiveTargets rootInfo rootTargetDir buckDirs programIndex fullModFileMap recursiveDirSet overrides)

buildModuleTargetGraph :: FilePath -> FilePath -> [FilePath] -> Maybe ModuleTargetOverrides -> IO BuildGraphOutput
buildModuleTargetGraph srcRoot rootBuckDir targetDirs overrides = do
  rootInfo <- normalizeRootDirectory srcRoot rootBuckDir
  rootedDirs <- mapM (resolveSourceDir rootInfo) targetDirs
  localModFileMap <- buildModuleFileMap rootedDirs
  fullModFileMap <- buildModuleFileMap [rootInfo.rootPath]
  buckDirs <- collectBuckDirectories rootInfo
  rootTargetDir <- resolveRootTargetDir rootInfo rootBuckDir
  programIndex <-
    case targetDirs of
      [] -> loadSimpleProgramIndex (Map.elems localModFileMap) fullModFileMap
      _ -> loadAllPrograms [srcRoot]
  let moduleInfos = buildModuleInfos rootInfo programIndex fullModFileMap
      activeOverrides = fromMaybe emptyTargetOverrides overrides
      hasOverrideMap = isJust overrides
  pure (moduleTargetsFromInfos rootInfo rootTargetDir buckDirs hasOverrideMap moduleInfos activeOverrides)

collectBuckDirectories :: RootDirectory -> IO (Set DirName)
collectBuckDirectories rootInfo = go rootPath
 where
  rootPath = rootInfo.rootPath

  go :: FilePath -> IO (Set DirName)
  go dir = do
    entries <- listDirectory dir
    let qualified = map (dir </>) entries
    buckHere <- doesFileExist (dir </> "BUCK")
    subDirs <- filterM doesDirectoryExist qualified
    childSets <- mapM go subDirs
    let relative = normalise (makeRelative rootPath dir)
        relTextRaw = pathToText relative
        relText = stripLeadingSlash relTextRaw
        dirText =
          if relative == "." || T.null relText
            then "."
            else applySourcePrefix rootInfo relText
        hereSet =
          if buckHere
            then Set.singleton (DirName dirText)
            else Set.empty
    pure (Set.unions (hereSet : childSets))

  stripLeadingSlash :: Text -> Text
  stripLeadingSlash = T.dropWhile (== '/')

loadSimpleProgramIndex :: [FilePath] -> ModFileMap -> IO ProgramIndex
loadSimpleProgramIndex files fullModFileMap =
  foldM
    ( \acc file -> do
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

buildModuleInfos :: RootDirectory -> ProgramIndex -> ModFileMap -> HashMap Hir.ModuleText ModuleInfo
buildModuleInfos rootInfo programIndex fullModFileMap =
  HM.fromList
    [ (moduleName, toInfo moduleName program)
    | (moduleName, program) <- HM.toList programIndex
    ]
 where
  toInfo :: Hir.ModuleText -> Hir.Read.Program -> ModuleInfo
  toInfo moduleName program =
    case HM.lookup moduleName fullModFileMap of
      Just filePath -> localInfo filePath
      Nothing -> externalInfo
   where
    localInfo filePath =
      ModuleInfo
        { name = moduleName
        , location = LocalModule (moduleDirectory rootInfo filePath)
        , imports = moduleImports
        , sourceFile = Just (pathToText filePath)
        , persistentModels = PersistentModels.requiredModelFiles program
        }
    externalInfo =
      ModuleInfo
        { name = moduleName
        , location = ExternalModule
        , imports = moduleImports
        , sourceFile = Nothing
        , persistentModels = PersistentModels.requiredModelFiles program
        }

    moduleImports =
      let directImports = (.mod) <$> program.imports
          globImports = globImportModules fullModFileMap moduleName program
       in Set.fromList (directImports ++ globImports)

initialBuildState :: RootDirectory -> DirName -> Set DirName -> ProgramIndex -> ModFileMap -> Set DirName -> Bool -> ModuleTargetOverrides -> BuildState
initialBuildState rootInfo rootTargetDir buckDirs programIndex fullModFileMap recursiveTargetDirs hasOverrideMap overrides =
  BuildState {moduleInfos, moduleToTarget, targets, rootInfo, preassignedTargets}
 where
  moduleInfos :: HashMap Hir.ModuleText ModuleInfo
  moduleInfos = buildModuleInfos rootInfo programIndex fullModFileMap

  preassignedTargets :: HashMap Hir.ModuleText TargetOverride
  preassignedTargets =
    HM.filterWithKey
      (\moduleName _ -> HM.member moduleName moduleInfos)
      overrideRaw

  assignments :: [(Hir.ModuleText, TargetKey, DirName)]
  assignments =
    mapMaybe
      ( \(moduleName, info) -> do
          (targetKey, targetDir) <- moduleTargetAssignment preassignedTargets info
          pure (moduleName, targetKey, targetDir)
      )
      (HM.toList moduleInfos)

  moduleToTarget :: HashMap Hir.ModuleText TargetKey
  moduleToTarget =
    HM.fromList
      [ (moduleName, targetKey)
      | (moduleName, targetKey, _) <- assignments
      ]

  targets :: HashMap TargetKey TargetState
  targets =
    HM.fromListWith
      mergeTargets
      [ ( targetKey
        , TargetState
            { key = targetKey
            , dir = targetDir
            , modules = Set.singleton moduleName
            }
        )
      | (moduleName, targetKey, targetDir) <- assignments
      ]

  ModuleTargetOverrides overrideRaw = overrides

  rootControlledDirs :: Set DirName
  rootControlledDirs =
    Set.unions
      [ dirClosure dir
      | (moduleName, override) <- HM.toList overrideRaw
      , isRootOverride override
      , dir <- overrideDirs moduleName (HM.lookup moduleName moduleInfos)
      ]

  rootControlledList :: [DirName]
  rootControlledList = Set.toList rootControlledDirs

  preservedDirTargets :: Set DirName
  preservedDirTargets =
    if hasOverrideMap
      then Set.fromList (mapMaybe overrideDirectory (HM.elems overrideRaw))
      else Set.empty

  moduleTargetAssignment :: HashMap Hir.ModuleText TargetOverride -> ModuleInfo -> Maybe (TargetKey, DirName)
  moduleTargetAssignment overridesMap info
    | HM.member info.name overridesMap = Nothing
    | hasOverrideMap = assignWithOverrides info
    | otherwise = assignWithoutOverrides info

  assignWithOverrides :: ModuleInfo -> Maybe (TargetKey, DirName)
  assignWithOverrides info =
    case info.location of
      LocalModule moduleDir ->
        let targetDir = fromMaybe rootTargetDir (nearestBuckDir moduleDir)
         in Just (DirectoryTarget targetDir, targetDir)
      ExternalModule -> Nothing

  assignWithoutOverrides :: ModuleInfo -> Maybe (TargetKey, DirName)
  assignWithoutOverrides info =
    case info.location of
      LocalModule moduleDir
        | Just buckDir <- nearestBuckDir moduleDir -> Just (DirectoryTarget buckDir, buckDir)
        | moduleIsRootControlled moduleDir -> Just (DirectoryTarget rootTargetDir, rootTargetDir)
        | shouldStickWithChildDir moduleDir -> Just (DirectoryTarget moduleDir, moduleDir)
        | otherwise ->
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
     in not (null ancestorSegments) && List.isPrefixOf ancestorSegments childSegments

  dirDepth :: DirName -> Int
  dirDepth = length . dirNameSegments

  dirNameSegments :: DirName -> [Text]
  dirNameSegments (DirName text) = textSegments text

  textSegments :: Text -> [Text]
  textSegments text
    | text == rootInfo.rootLabel = []
    | text == "." = []
    | otherwise = filter (not . T.null) (T.splitOn "/" text)

  overrideDirectory :: TargetOverride -> Maybe DirName
  overrideDirectory (TargetOverride overrideText) = do
    remaining <- T.stripPrefix "root//src/" overrideText
    let (pathText, rest) = T.breakOn ":" remaining
    if T.null pathText || T.null rest
      then Nothing
      else
        let normalized = pathToText (T.unpack pathText)
            prefixed = applySourcePrefix rootInfo normalized
         in if T.null normalized then Nothing else Just (DirName prefixed)

  isRootOverride :: TargetOverride -> Bool
  isRootOverride (TargetOverride overrideText) =
    case T.stripPrefix "root//" overrideText of
      Just rest -> ":" `T.isPrefixOf` rest
      Nothing -> False

  moduleIsRootControlled :: DirName -> Bool
  moduleIsRootControlled moduleDir =
    any
      ( \rootDir ->
          case dirNameToText rootDir of
            "." -> True
            _ -> rootDir == moduleDir || dirIsAncestorOf rootDir moduleDir
      )
      rootControlledList

  overrideDirs :: Hir.ModuleText -> Maybe ModuleInfo -> [DirName]
  overrideDirs moduleName maybeInfo =
    let nameDir = moduleNameDirectory moduleName
        infoDir =
          case maybeInfo of
            Just ModuleInfo {location = LocalModule moduleDir} -> Just moduleDir
            _ -> Nothing
     in Set.toList (Set.fromList (map normalizeDir (catMaybes [nameDir, infoDir])))

  moduleNameDirectory :: Hir.ModuleText -> Maybe DirName
  moduleNameDirectory moduleName =
    case NE.init (moduleName.parts) of
      [] -> Just (DirName rootInfo.rootLabel)
      dirParts -> Just (DirName (applySourcePrefix rootInfo (T.intercalate "/" dirParts)))

  shouldStickWithChildDir :: DirName -> Bool
  shouldStickWithChildDir dir =
    hasOverrideMap
      && Set.member dir preservedDirTargets
      && not (Set.member dir recursiveTargetDirs)

  normalizeDir :: DirName -> DirName
  normalizeDir (DirName text)
    | trimmed == "" = DirName "."
    | otherwise = DirName (reapplyPrefix rootInfo trimmed)
   where
    trimmed = stripSourcePrefix rootInfo text

  dirClosure :: DirName -> Set DirName
  dirClosure dir =
    let rel = stripSourcePrefix rootInfo (dirNameToText dir)
        segments = filter (not . T.null) (T.splitOn "/" rel)
        dirs =
          case segments of
            [] -> [DirName "."]
            _ ->
              [ DirName (reapplyPrefix rootInfo (T.intercalate "/" (take k segments)))
              | k <- [1 .. length segments]
              ]
     in Set.fromList dirs

  nearestBuckDir :: DirName -> Maybe DirName
  nearestBuckDir = nearestBuckDirFor rootInfo buckDirs

moduleDirectory :: RootDirectory -> FilePath -> DirName
moduleDirectory rootInfo filePath =
  let dirPath = normalise (takeDirectory filePath)
   in DirName (directoryText rootInfo dirPath)

normalizeRootDirectory :: FilePath -> FilePath -> IO RootDirectory
normalizeRootDirectory path rootBuck = do
  absRoot <- makeAbsolute path
  absBuck <- makeAbsolute rootBuck
  let normalizedRoot = dropTrailingPathSeparator (normalise absRoot)
      normalizedBuck = dropTrailingPathSeparator (normalise absBuck)
      relativeRaw = makeRelative normalizedBuck normalizedRoot
      relative = normalise relativeRaw
      prefixText = pathToText relative
      trimmed = trimTrailingSlash prefixText
      sourcePrefix =
        if T.null trimmed || trimmed == "."
          then Nothing
          else Just trimmed
      label = maybe "." ensureTrailingSlash sourcePrefix
  pure RootDirectory {rootPath = normalizedRoot, rootLabel = label, sourcePrefix}

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

applySourcePrefix :: RootDirectory -> Text -> Text
applySourcePrefix rootInfo rel =
  case rootInfo.sourcePrefix of
    Nothing -> rel
    Just prefix -> ensureTrailingSlash prefix <> rel

stripSourcePrefix :: RootDirectory -> Text -> Text
stripSourcePrefix rootInfo text
  | text == "." = ""
  | otherwise =
      let trimmed = trimTrailingSlash text
       in case rootInfo.sourcePrefix of
            Nothing -> T.dropWhile (== '/') trimmed
            Just prefix ->
              let prefixTrimmed = trimTrailingSlash prefix
                  prefixed = ensureTrailingSlash prefixTrimmed
                  trimmedWithSlash = ensureTrailingSlash trimmed
               in case T.stripPrefix prefixed trimmedWithSlash of
                    Just rest -> T.dropWhile (== '/') rest
                    Nothing ->
                      if trimmed == prefixTrimmed
                        then ""
                        else T.dropWhile (== '/') trimmed

reapplyPrefix :: RootDirectory -> Text -> Text
reapplyPrefix rootInfo rel
  | T.null rel = "."
  | otherwise = applySourcePrefix rootInfo rel

nearestBuckDirFor :: RootDirectory -> Set DirName -> DirName -> Maybe DirName
nearestBuckDirFor rootInfo buckDirs dir = find (`Set.member` buckDirs) (dirLineageFrom rootInfo dir)

dirLineageFrom :: RootDirectory -> DirName -> [DirName]
dirLineageFrom rootInfo dir = dir : maybe [] (dirLineageFrom rootInfo) (dirParentOf rootInfo dir)

dirParentOf :: RootDirectory -> DirName -> Maybe DirName
dirParentOf rootInfo current =
  let currentText = dirNameToText current
      rel = stripSourcePrefix rootInfo currentText
      segments = filter (not . T.null) (T.splitOn "/" rel)
   in case segments of
        [] ->
          if currentText == "."
            then Nothing
            else Just (DirName ".")
        [_] -> Just (DirName ".")
        _ ->
          let parentSegs = take (length segments - 1) segments
           in Just (DirName (reapplyPrefix rootInfo (T.intercalate "/" parentSegs)))

resolveRootTargetDir :: RootDirectory -> FilePath -> IO DirName
resolveRootTargetDir _ _ = pure (DirName ".")

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
        ( \dep -> case HM.lookup dep state.moduleToTarget of
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
                      DirectoryTarget parentDir -> parentDir
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
    ( \(depMap, edgeMap) key targetState ->
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
    ( \(depSet, edgeMap) moduleName ->
        case HM.lookup moduleName state.moduleInfos of
          Nothing -> (depSet, edgeMap)
          Just info ->
            foldl'
              ( \(accSet, accMap) depName ->
                  case HM.lookup depName state.moduleToTarget of
                    Just depKey
                      | depKey /= key ->
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

modulePersistentEntries :: HashMap Hir.ModuleText ModuleInfo -> Set Hir.ModuleText -> [(Hir.ModuleText, (Text, [Text]))]
modulePersistentEntries infoMap moduleNames =
  List.sortOn (moduleTextToText . fst) (mapMaybe entry (Set.toList moduleNames))
 where
  entry moduleName = do
    info <- HM.lookup moduleName infoMap
    dep <- modulePersistentSrcDeps moduleName info
    pure (moduleName, dep)

modulePersistentSrcDeps :: Hir.ModuleText -> ModuleInfo -> Maybe (Text, [Text])
modulePersistentSrcDeps moduleName info =
  case (moduleHasPersistentModels moduleName, info.sourceFile, info.persistentModels) of
    (True, Just filePath, models@(_ : _)) ->
      Just (filePath, map addModelSuffix models)
    _ -> Nothing

moduleHasPersistentModels :: Hir.ModuleText -> Bool
moduleHasPersistentModels moduleName =
  any (== "PersistentModels") (NE.toList moduleName.parts)

addModelSuffix :: Text -> Text
addModelSuffix model = persistentModelsRoot <> "/" <> suffixed
 where
  suffixed
    | ".persistentmodels" `T.isSuffixOf` model = model
    | otherwise = model <> ".persistentmodels"

buildGraphNodes :: BuildState -> HashMap TargetKey (Set TargetKey) -> [(TargetKey, TargetKey, [TargetKey])]
buildGraphNodes state deps =
  [ (key, key, Set.toList (HM.lookupDefault Set.empty key deps))
  | key <- HM.keys state.targets
  ]

-- Finalization ----------------------------------------------------------------

validateTargetAssignments :: BuildState -> Either BuildGraphError ()
validateTargetAssignments state = do
  generatedAssignments <- foldM collectAssignments HM.empty (HM.toList state.targets)
  mapM_ (checkPreassigned generatedAssignments) (HM.toList state.preassignedTargets)
 where
  collectAssignments :: HashMap Hir.ModuleText ModuleTargetAssignment -> (TargetKey, TargetState) -> Either BuildGraphError (HashMap Hir.ModuleText ModuleTargetAssignment)
  collectAssignments acc (targetKey, targetState) =
    foldM (assignModule targetKey) acc (Set.toList targetState.modules)

  assignModule :: TargetKey -> HashMap Hir.ModuleText ModuleTargetAssignment -> Hir.ModuleText -> Either BuildGraphError (HashMap Hir.ModuleText ModuleTargetAssignment)
  assignModule targetKey acc moduleName =
    case HM.lookup moduleName acc of
      Nothing -> pure (HM.insert moduleName (AssignedTarget targetKey) acc)
      Just existing ->
        Left (ModuleAssignedMultipleTargets moduleName (existing :| [AssignedTarget targetKey]))

  checkPreassigned :: HashMap Hir.ModuleText ModuleTargetAssignment -> (Hir.ModuleText, TargetOverride) -> Either BuildGraphError ()
  checkPreassigned generated (moduleName, override) =
    case HM.lookup moduleName generated of
      Nothing -> pure ()
      Just existing ->
        Left (ModuleAssignedMultipleTargets moduleName (existing :| [PreassignedTarget override]))

finalizeGraph :: BuildState -> MaxDirTargetGraph
finalizeGraph state =
  let moduleDeps = computeTargetModuleDependencies state
      materialize targetState =
        Target
          { key = targetState.key
          , dir = targetState.dir
          , modules = targetState.modules
          , dependsOn = HM.lookupDefault Set.empty targetState.key moduleDeps
          , persistentSrcDeps = persistentEntries targetState
          }
      persistentEntries targetState =
        modulePersistentEntries state.moduleInfos targetState.modules
      targets = HM.map materialize state.targets
      generatedAssignments = AssignedTarget <$> state.moduleToTarget
      preassignedAssignments = PreassignedTarget <$> state.preassignedTargets
      moduleAssignments = HM.union generatedAssignments preassignedAssignments
   in MaxDirTargetGraph {targets, moduleToTarget = moduleAssignments, rootInfo = state.rootInfo}

moduleTargetsFromInfos :: RootDirectory -> DirName -> Set DirName -> Bool -> HashMap Hir.ModuleText ModuleInfo -> ModuleTargetOverrides -> BuildGraphOutput
moduleTargetsFromInfos rootInfo rootTargetDir buckDirs hasOverrideMap moduleInfos overrides =
  BuildGraphOutput
    { targets = map mkTarget sortedLocalModules
    , moduleToTarget = HM.union localAssignments overrideAssignments
    }
 where
  ModuleTargetOverrides overrideMap = overrides

  sortedLocalModules :: [(Hir.ModuleText, ModuleInfo, DirName)]
  sortedLocalModules =
    sortOn
      (moduleTextToText . selectModule)
      [ (moduleName, info, effectiveDir moduleDir)
      | (moduleName, info) <- HM.toList moduleInfos
      , LocalModule moduleDir <- [info.location]
      , not (HM.member moduleName overrideMap)
      ]
   where
    selectModule (moduleName, _, _) = moduleName

  effectiveDir :: DirName -> DirName
  effectiveDir moduleDir
    | hasOverrideMap = fromMaybe rootTargetDir (nearestBuckDirFor rootInfo buckDirs moduleDir)
    | otherwise = moduleDir

  mkTarget :: (Hir.ModuleText, ModuleInfo, DirName) -> TargetOutput
  mkTarget (moduleName, info, dir) =
    let key = ModuleTargetOutput (moduleName.text)
        slug = moduleTargetSlug (moduleTextToText moduleName)
        srcDeps = maybe [] pure (modulePersistentSrcDeps moduleName info)
     in TargetOutput
          { key = key
          , targetName = slug
          , targetKey = renderName rootInfo dir moduleName
          , directory = dirNameToText dir
          , modules = [moduleTextToText moduleName]
          , dependsOn = map moduleTextToText (Set.toList (moduleDependencies info))
          , srcDeps = srcDeps
          }

  moduleDependencies :: ModuleInfo -> Set Hir.ModuleText
  moduleDependencies info = Set.delete info.name info.imports

  localAssignments :: HashMap Text Text
  localAssignments =
    HM.fromList
      [ (moduleTextToText moduleName, renderName rootInfo dir moduleName)
      | (moduleName, _, dir) <- sortedLocalModules
      ]

  overrideAssignments :: HashMap Text Text
  overrideAssignments =
    HM.fromList
      [
        ( moduleTextToText moduleName
        , targetNameFromKeyOutput (ExternalTargetOutput target)
        )
      | (moduleName, TargetOverride target) <- HM.toList overrideMap
      , HM.member moduleName moduleInfos
      ]

graphToOutput :: MaxDirTargetGraph -> BuildGraphOutput
graphToOutput graph =
  BuildGraphOutput
    { targets = targetOutputs
    , moduleToTarget = moduleAssignments
    }
 where
  rootInfo = graph.rootInfo

  targetPairs :: [(TargetKey, TargetOutput)]
  targetPairs =
    [ (target.key, targetToOutput target)
    | target <- HM.elems graph.targets
    ]

  targetOutputs :: [TargetOutput]
  targetOutputs = map snd targetPairs

  targetNameMap :: HashMap TargetKey Text
  targetNameMap = HM.fromList [(key, output.targetKey) | (key, output) <- targetPairs]

  targetToOutput :: Target -> TargetOutput
  targetToOutput target =
    let keyOut = keyOutput target.key
        (shortName, fullName) =
          case target.key of
            ModuleTarget moduleName -> moduleTargetNames moduleName target.dir
            _ ->
              let base = targetNameFromKeyOutput keyOut
               in (base, base)
        srcDeps = map snd target.persistentSrcDeps
     in TargetOutput
          { key = keyOut
          , targetName = shortName
          , targetKey = fullName
          , directory = dirNameToText target.dir
          , modules = map qualifiedModuleName (Set.toList target.modules)
          , dependsOn = map qualifiedModuleName (Set.toList target.dependsOn)
          , srcDeps = srcDeps
          }

  moduleTargetNames :: Hir.ModuleText -> DirName -> (Text, Text)
  moduleTargetNames moduleName dir =
    let slug = moduleTargetSlug (moduleTextToText moduleName)
     in (slug, renderName rootInfo dir moduleName)

  qualifiedModuleName :: Hir.ModuleText -> Text
  qualifiedModuleName moduleName = moduleName.text

  moduleAssignments :: HashMap Text Text
  moduleAssignments =
    HM.fromList
      [ (qualifiedModuleName moduleName, assignmentTargetName assignment)
      | (moduleName, assignment) <- HM.toList graph.moduleToTarget
      ]

  keyOutput :: TargetKey -> TargetKeyOutput
  keyOutput key =
    case key of
      DirectoryTarget dirName -> DirectoryTargetOutput (dirNameToText dirName)
      RecursiveDirectoryTarget dirName -> RecursiveDirectoryTargetOutput (dirNameToText dirName)
      ModuleTarget moduleName -> ModuleTargetOutput moduleName.text

  assignmentTargetName :: ModuleTargetAssignment -> Text
  assignmentTargetName assignment =
    case assignment of
      AssignedTarget key -> HM.lookupDefault (targetNameFromKeyOutput (keyOutput key)) key targetNameMap
      PreassignedTarget (TargetOverride target) -> target

renderBuildGraphError :: BuildGraphError -> String
renderBuildGraphError err =
  case err of
    ModuleCycleDetected edges ->
      let header = "Unable to build acyclic target graph; cycle detected:\n"
          body = unlines (map formatEdge (NE.toList edges))
       in header <> body
    ModuleAssignedMultipleTargets moduleName assignments ->
      let header = "Module assigned to multiple targets:\n"
          body =
            T.unpack (moduleTextToText moduleName)
              <> " -> "
              <> assignmentSummary assignments
       in header <> body <> "\n"
 where
  formatEdge ModuleCycleEdge {sourceModule, importedModule} =
    T.unpack (moduleTextToText sourceModule)
      <> " imports "
      <> T.unpack (moduleTextToText importedModule)

  assignmentSummary :: NonEmpty ModuleTargetAssignment -> String
  assignmentSummary assignments =
    T.unpack (T.intercalate ", " (NE.toList (fmap assignmentLabel assignments)))

  assignmentLabel :: ModuleTargetAssignment -> Text
  assignmentLabel assignment =
    case assignment of
      AssignedTarget key -> describeTargetKey key
      PreassignedTarget (TargetOverride overrideText) -> "preassigned:" <> overrideText

  describeTargetKey :: TargetKey -> Text
  describeTargetKey key =
    case key of
      DirectoryTarget dirName -> "directory:" <> dirNameToText dirName
      RecursiveDirectoryTarget dirName -> "recursiveDirectory:" <> dirNameToText dirName
      ModuleTarget moduleName -> "module:" <> moduleName.text

parseProgramFromFile :: FilePath -> IO Hir.Read.Program
parseProgramFromFile file = do
  bytes <- BS.readFile file
  let (_src, prg) = parsePrg (TE.decodeUtf8 bytes)
  pure prg

directoryText :: RootDirectory -> FilePath -> Text
directoryText root dirPath =
  let relativeRaw = makeRelative root.rootPath dirPath
      relative = normalise relativeRaw
   in case () of
        _
          | relative == "." || null relative -> root.rootLabel
          | isOutsideRoot relative -> pathToText dirPath
          | otherwise -> formatInside relative
 where
  isOutsideRoot rel = ".." `List.isPrefixOf` rel

  formatInside rel =
    let relText = stripLeadingSlashes (pathToText rel)
     in case root.sourcePrefix of
          Nothing -> relText
          Just prefix -> ensureTrailingSlash prefix <> relText

ensureTrailingSlash :: Text -> Text
ensureTrailingSlash txt
  | T.null txt = txt
  | T.isSuffixOf "/" txt = txt
  | otherwise = txt <> "/"

pathToText :: FilePath -> Text
pathToText fp =
  let normalized = normalise fp
      sanitized = map replaceSeparator normalized
      result = case sanitized of
        [] -> "."
        "." -> "."
        other -> other
   in T.pack result
 where
  replaceSeparator c
    | isPathSeparator c = '/'
    | otherwise = c
