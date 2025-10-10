{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildGraph.Directory where

import Arborist.Files (ModFileMap, buildModuleFileMap)
import Arborist.ProgramIndex (ProgramIndex)
import Control.Monad (forM)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Graph (SCC (..), stronglyConnComp)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import HaskellAnalyzer (parsePrg)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir

-- | Error cases encountered while attempting to build a maximal acyclic graph
data BuildGraphError
  = ModuleCycleDetected (NonEmpty Hir.ModuleText)
  deriving (Eq, Show)

-- | Canonical representation of a directory (split on module components)
newtype DirName = DirName {segments :: [Text]}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show DirName where
  show (DirName []) = "."
  show (DirName segs) = T.unpack (T.intercalate "." segs)

-- | Identifier for targets in the directory graph
data TargetKey
  = DirectoryTarget DirName
  | ModuleTarget Hir.ModuleText
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Finalized target representation exposed to callers
data Target = Target
  { key :: TargetKey
  , dir :: DirName
  , modules :: Set Hir.ModuleText
  , dependsOn :: Set TargetKey
  }
  deriving (Eq, Show)

-- | Complete target graph once all cycles have been resolved
newtype MaxDirTargetGraph = MaxDirTargetGraph
  { targets :: HashMap TargetKey Target
  }
  deriving (Eq, Show)

-- Internal representations ----------------------------------------------------

data ModuleInfo = ModuleInfo
  { name :: Hir.ModuleText
  , dir :: DirName
  , imports :: Set Hir.ModuleText
  }

data TargetState = TargetState
  { key :: TargetKey
  , dir :: DirName
  , modules :: Set Hir.ModuleText
  }

data BuildState = BuildState
  { moduleInfos :: HashMap Hir.ModuleText ModuleInfo
  , moduleToTarget :: HashMap Hir.ModuleText TargetKey
  , targets :: HashMap TargetKey TargetState
  }

-- Public API -----------------------------------------------------------------

buildMaxDirTargetGraph :: ProgramIndex -> Either BuildGraphError MaxDirTargetGraph
buildMaxDirTargetGraph programIndex = do
  buildState <- resolveDirectoryTargets (initialBuildState programIndex)
  pure (finalizeGraph buildState)

buildGraphFromDirectories :: [FilePath] -> IO (Either BuildGraphError MaxDirTargetGraph)
buildGraphFromDirectories sourceDirs = do
  modFileMap <- buildModuleFileMap sourceDirs
  programIndex <- loadProgramIndex modFileMap
  pure (buildMaxDirTargetGraph programIndex)

loadProgramIndex :: ModFileMap -> IO ProgramIndex
loadProgramIndex modFileMap = do
  pairs <- forM (HM.toList modFileMap) $ \(modText, file) -> do
    program <- parseProgramFromFile file
    pure (modText, program)
  pure (HM.fromList pairs)

-- Construction ----------------------------------------------------------------

initialBuildState :: ProgramIndex -> BuildState
initialBuildState programIndex =
  let moduleInfos =
        HM.fromList
          [ (moduleName, toInfo moduleName program)
          | (moduleName, program) <- HM.toList programIndex
          ]

      moduleToTarget =
        HM.fromList
          [ (info.name, DirectoryTarget info.dir)
          | info <- HM.elems moduleInfos
          ]

      dirGroups =
        foldl'
          (\acc info -> HM.insertWith Set.union info.dir (Set.singleton info.name) acc)
          HM.empty
          (HM.elems moduleInfos)

      targets =
        HM.fromList
          [ (key, TargetState {key, dir, modules})
          | (dir, modules) <- HM.toList dirGroups
          , let key = DirectoryTarget dir
          ]
   in BuildState {moduleInfos, moduleToTarget, targets}
 where
  toInfo :: Hir.ModuleText -> Hir.Read.Program -> ModuleInfo
  toInfo moduleName program =
    ModuleInfo
      { name = moduleName
      , dir = moduleDirectory moduleName
      , imports = Set.fromList ((.mod) <$> program.imports)
      }

moduleDirectory :: Hir.ModuleText -> DirName
moduleDirectory moduleText =
  let parts = NE.toList moduleText.parts
   in DirName (take (length parts - 1) parts)

-- Cycle resolution ------------------------------------------------------------

resolveDirectoryTargets :: BuildState -> Either BuildGraphError BuildState
resolveDirectoryTargets = go
 where
  go state =
    let deps = computeTargetDependencies state
        nodes = buildGraphNodes state deps
        cycles = [vertices | CyclicSCC vertices <- stronglyConnComp nodes]
     in case cycles of
          [] -> Right state
          _ ->
            let (state', changed) = foldl' splitCycle (state, False) cycles
             in if changed
                  then go state'
                  else Left (ModuleCycleDetected (pickCycleModules state cycles))

splitCycle :: (BuildState, Bool) -> [TargetKey] -> (BuildState, Bool)
splitCycle (state, changed) vertices =
  let cycleSet = Set.fromList vertices
   in foldl'
        (splitTarget cycleSet)
        (state, changed)
        vertices

splitTarget :: Set TargetKey -> (BuildState, Bool) -> TargetKey -> (BuildState, Bool)
splitTarget cycleSet (state, changed) key =
  case HM.lookup key state.targets of
    Nothing -> (state, changed)
    Just targetState ->
      case key of
        ModuleTarget _ -> (state, changed)
        DirectoryTarget _ ->
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

          moduleInfo =
            fromMaybe
              (error "Missing module info while splitting targets")
              (HM.lookup moduleName state.moduleInfos)
          newKey = ModuleTarget moduleName
          newTarget =
            TargetState
              { key = newKey
              , dir = moduleInfo.dir
              , modules = Set.singleton moduleName
              }
          targetsWithNew = HM.insert newKey newTarget targetsWithoutModule
          moduleToTarget = HM.insert moduleName newKey state.moduleToTarget
       in state
            { targets = targetsWithNew
            , moduleToTarget
            }

withModules :: TargetState -> Set Hir.ModuleText -> TargetState
withModules targetState newModules =
  TargetState
    { key = targetState.key
    , dir = targetState.dir
    , modules = newModules
    }

pickCycleModules :: BuildState -> [[TargetKey]] -> NonEmpty Hir.ModuleText
pickCycleModules state (cycleVertices : _) =
  case NE.nonEmpty (concatMap (targetModules state) cycleVertices) of
    Just ne -> ne
    Nothing -> fallback cycleVertices
 where
  fallback (vertex : _) =
    case HM.lookup vertex state.targets of
      Just targetState ->
        case Set.toList targetState.modules of
          moduleName : _ -> moduleName :| []
          [] -> error "Cycle target without modules"
      Nothing -> error "Missing target while reporting cycle"
  fallback [] = error "Empty cycle encountered"
pickCycleModules _ [] = error "Internal error: pickCycleModules with no cycles"

targetModules :: BuildState -> TargetKey -> [Hir.ModuleText]
targetModules state key =
  case HM.lookup key state.targets of
    Just targetState -> Set.toList targetState.modules
    Nothing -> []

-- Dependency analysis ---------------------------------------------------------

computeTargetDependencies :: BuildState -> HashMap TargetKey (Set TargetKey)
computeTargetDependencies state =
  HM.mapWithKey (collectDeps state) state.targets

collectDeps :: BuildState -> TargetKey -> TargetState -> Set TargetKey
collectDeps state key targetState =
  Set.unions (moduleDeps <$> Set.toList targetState.modules)
 where
  moduleDeps moduleName =
    case HM.lookup moduleName state.moduleInfos of
      Nothing -> Set.empty
      Just info ->
        Set.fromList
          [ depKey
          | depName <- Set.toList info.imports
          , Just depKey <- [HM.lookup depName state.moduleToTarget]
          , depKey /= key
          ]

buildGraphNodes :: BuildState -> HashMap TargetKey (Set TargetKey) -> [(TargetKey, TargetKey, [TargetKey])]
buildGraphNodes state deps =
  [ (key, key, Set.toList (HM.lookupDefault Set.empty key deps))
  | key <- HM.keys state.targets
  ]

-- Finalization ----------------------------------------------------------------

finalizeGraph :: BuildState -> MaxDirTargetGraph
finalizeGraph state =
  let deps = computeTargetDependencies state
      materialize targetState =
        Target
          { key = targetState.key
          , dir = targetState.dir
          , modules = targetState.modules
          , dependsOn = HM.lookupDefault Set.empty targetState.key deps
          }
      targets = HM.map materialize state.targets
   in MaxDirTargetGraph {targets}

graphToJson :: MaxDirTargetGraph -> Aeson.Value
graphToJson graph =
  Aeson.object
    [ "targets" .= map targetToJson (HM.elems graph.targets)
    ]
 where
  targetToJson :: Target -> Aeson.Value
  targetToJson target =
    Aeson.object
      [ "key" .= targetKeyToJson target.key
      , "directory" .= dirNameToText target.dir
      , "modules" .= map moduleTextToText (Set.toList target.modules)
      , "dependsOn" .= map targetKeyToJson (Set.toList target.dependsOn)
      ]

renderBuildGraphError :: BuildGraphError -> String
renderBuildGraphError (ModuleCycleDetected mods) =
  "Unable to build acyclic target graph; modules participating in cycle: "
    <> T.unpack (T.intercalate ", " (map moduleTextToText (NE.toList mods)))

targetKeyToJson :: TargetKey -> Aeson.Value
targetKeyToJson key =
  case key of
    DirectoryTarget dirName ->
      Aeson.object
        [ "type" .= Aeson.String "directory"
        , "name" .= dirNameToText dirName
        ]
    ModuleTarget moduleName ->
      Aeson.object
        [ "type" .= Aeson.String "module"
        , "name" .= moduleTextToText moduleName
        ]

dirNameToText :: DirName -> Text
dirNameToText (DirName segments) =
  case segments of
    [] -> "."
    _ -> T.intercalate "." segments

moduleTextToText :: Hir.ModuleText -> Text
moduleTextToText = (.text)

parseProgramFromFile :: FilePath -> IO Hir.Read.Program
parseProgramFromFile file = do
  bytes <- BS.readFile file
  let (_src, prg) = parsePrg (TE.decodeUtf8 bytes)
  pure prg
