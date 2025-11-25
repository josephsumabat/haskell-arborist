{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildGraph.GroupCandidates (groupOutputCandidates) where

import BuildGraph.Directory (
  BuildGraphOutput (..),
  DirName (..),
  TargetKeyOutput (..),
  TargetOutput (..),
 )
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

newtype TargetNodeId = TargetNodeId Text
  deriving (Eq, Ord, Show, Hashable)

targetNodeIdText :: TargetNodeId -> Text
targetNodeIdText (TargetNodeId txt) = txt

data MergeStrategy
  = MergeEntireDirectory
  | MergeByModulePrefixes [Text]

data OutputGraphState = OutputGraphState
  { ogTargets :: !(HashMap TargetNodeId OutputTargetState)
  , ogModuleToTarget :: !(HashMap Text Text)
  , ogTargetDeps :: !(HashMap TargetNodeId (Set TargetNodeId))
  , ogReverseTargetDeps :: !(HashMap TargetNodeId (Set TargetNodeId))
  }

data OutputTargetState = OutputTargetState
  { otKey :: !TargetKeyOutput
  , otKeyId :: !TargetNodeId
  , otTargetName :: !Text
  , otDirectory :: !Text
  , otModules :: !(Set Text)
  , otDependsOn :: !(Set Text)
  }

groupOutputCandidates :: BuildGraphOutput -> [DirName] -> [Text] -> BuildGraphOutput
groupOutputCandidates output requestedDirs modulePrefixes =
  stateToOutput finalState
 where
  mergeStrategy =
    case dedupePreservingOrder modulePrefixes of
      [] -> MergeEntireDirectory
      prefixes -> MergeByModulePrefixes prefixes
  requestedSet =
    case requestedDirs of
      [] -> Nothing
      _ -> Just (Set.fromList (map dirNameText requestedDirs))
  initialState = outputToState output
  availableDirs = collectGroupableDirectories initialState
  targetDirs =
    maybe availableDirs (`Set.intersection` availableDirs) requestedSet
  finalState =
    foldl' (mergeDir mergeStrategy) initialState (List.sort (Set.toList targetDirs))

  dirNameText (DirName txt) = txt

dedupePreservingOrder :: (Ord a) => [a] -> [a]
dedupePreservingOrder = go Set.empty
 where
  go _ [] = []
  go seen (x : xs)
    | Set.member x seen = go seen xs
    | otherwise = x : go (Set.insert x seen) xs

mergeDir :: MergeStrategy -> OutputGraphState -> Text -> OutputGraphState
mergeDir strategy state dir =
  case strategy of
    MergeEntireDirectory -> mergeTargetsInDir state dir (const True)
    MergeByModulePrefixes prefixes ->
      let exploded = explodeDirectoryModules state dir
       in foldl'
            (\st prefix -> mergeTargetsInDir st dir (targetMatchesPrefix prefix))
            exploded
            prefixes

explodeDirectoryModules :: OutputGraphState -> Text -> OutputGraphState
explodeDirectoryModules state dir
  | null targetsToSplit = state
  | otherwise =
      recomputeAllDependencies
        state
          { ogTargets = targetsWithSplits
          , ogModuleToTarget = moduleAssignments'
          }
 where
  targetsToSplit =
    [ (targetId, target)
    | (targetId, target) <- HM.toList state.ogTargets
    , target.otDirectory == dir
    , targetHasModules target
    ]

  targetsWithoutOriginal = foldl' (flip HM.delete) state.ogTargets (map fst targetsToSplit)

  (moduleAssignments', targetsWithSplits) =
    foldl'
      insertSplitTarget
      (state.ogModuleToTarget, targetsWithoutOriginal)
      targetsToSplit

  insertSplitTarget (moduleAcc, targetAcc) (_, target) =
    foldl'
      (\(mAcc, tAcc) moduleName ->
          let newKey = ModuleTargetOutput moduleName
              newId = targetKeyId newKey
              newTargetName = renderOutputTargetName target.otDirectory moduleName
              newTarget =
                OutputTargetState
                  { otKey = newKey
                  , otKeyId = newId
                  , otTargetName = newTargetName
                  , otDirectory = target.otDirectory
                  , otModules = Set.singleton moduleName
                  , otDependsOn = target.otDependsOn
                  }
           in ( HM.insert moduleName newTargetName mAcc
              , HM.insert newId newTarget tAcc
              )
      )
      (moduleAcc, targetAcc)
      (Set.toList target.otModules)

mergeTargetsInDir :: OutputGraphState -> Text -> (OutputTargetState -> Bool) -> OutputGraphState
mergeTargetsInDir state dir predicate =
  mergeModuleIds state dir orderedIds
 where
  moduleTargets =
    List.sortOn (.otKeyId)
      [ target
      | target <- HM.elems state.ogTargets
      , target.otDirectory == dir
      , isModuleKey target.otKey
      , predicate target
      ]
  canonicalId = canonicalTargetId state moduleTargets
  moduleIds = map (.otKeyId) moduleTargets
  orderedIds = reorderCanonical canonicalId moduleIds

mergeModuleIds :: OutputGraphState -> Text -> [TargetNodeId] -> OutputGraphState
mergeModuleIds state dir moduleIds = finalState
 where
  moduleCount = length moduleIds
  totalAttempts =
    if moduleCount < 2 then 0 else moduleCount * (moduleCount - 1) `div` 2
  (finalState, _) = go state moduleIds 0
  go currentState [] attemptIndex = (currentState, attemptIndex)
  go currentState (destId : rest) attemptIndex =
    let (nextState, remainingRev, nextAttemptIndex) =
          foldl'
            ( \(st, acc, count) sourceId ->
                let attemptNo = count + 1
                 in case mergeModuleTargets st dir destId sourceId of
                      Just mergedState ->
                        ( trace (formatMergeResult dir destId sourceId True attemptNo totalAttempts) mergedState
                        , acc
                        , attemptNo
                        )
                      Nothing ->
                        ( trace (formatMergeResult dir destId sourceId False attemptNo totalAttempts) st
                        , sourceId : acc
                        , attemptNo
                        )
            )
            (currentState, [], attemptIndex)
            rest
        remaining = reverse remainingRev
     in go nextState remaining nextAttemptIndex

targetMatchesPrefix :: Text -> OutputTargetState -> Bool
targetMatchesPrefix prefix target =
  not (Text.null prefix)
    && not (Set.null target.otModules)
    && all (moduleMatchesPrefix prefix) (Set.toList target.otModules)

moduleMatchesPrefix :: Text -> Text -> Bool
moduleMatchesPrefix prefix moduleName
  | prefix == moduleName = True
  | otherwise = Text.isPrefixOf (prefixWithDot prefix) moduleName
 where
  prefixWithDot txt =
    if Text.null txt
      then txt
      else txt <> "."

canonicalTargetId :: OutputGraphState -> [OutputTargetState] -> Maybe TargetNodeId
canonicalTargetId state targets =
  case pickLeaf sortedLeaves of
    Just targetId -> Just targetId
    Nothing -> pickLeaf sortedAll
 where
  targetInfos = mapMaybe targetPrimary targets
  idSet = Set.fromList (map snd targetInfos)
  leaves =
    [ info
    | info@(_, targetId) <- targetInfos
    , let deps = HM.lookupDefault Set.empty targetId state.ogTargetDeps
    , Set.null (Set.intersection idSet deps)
    ]
  sortedLeaves = List.sortOn fst leaves
  sortedAll = List.sortOn fst targetInfos

  pickLeaf [] = Nothing
  pickLeaf ((_, tid) : _) = Just tid

targetPrimary :: OutputTargetState -> Maybe (Text, TargetNodeId)
targetPrimary target = do
  moduleName <- targetPrimaryModule target
  pure (moduleName, target.otKeyId)

targetPrimaryModule :: OutputTargetState -> Maybe Text
targetPrimaryModule target =
  case List.sort (Set.toList target.otModules) of
    [] -> Nothing
    (m : _) -> Just m

reorderCanonical :: Maybe TargetNodeId -> [TargetNodeId] -> [TargetNodeId]
reorderCanonical Nothing ids = ids
reorderCanonical (Just canonical) ids = matches ++ rest
 where
  (matches, rest) = List.partition (== canonical) ids

collectGroupableDirectories :: OutputGraphState -> Set Text
collectGroupableDirectories state =
  Set.fromList
    [ target.otDirectory
    | target <- HM.elems state.ogTargets
    , targetHasModules target
    ]

targetHasModules :: OutputTargetState -> Bool
targetHasModules target = not (Set.null target.otModules)

mergeModuleTargets :: OutputGraphState -> Text -> TargetNodeId -> TargetNodeId -> Maybe OutputGraphState
mergeModuleTargets state dir destId sourceId = do
  destTarget <- HM.lookup destId state.ogTargets
  sourceTarget <- HM.lookup sourceId state.ogTargets
  if destTarget.otDirectory /= dir || sourceTarget.otDirectory /= dir
    then Nothing
    else
      let mergedModules = Set.union destTarget.otModules sourceTarget.otModules
          combinedDeps = Set.union destTarget.otDependsOn sourceTarget.otDependsOn
          filteredDeps = Set.difference combinedDeps mergedModules
          targetBase =
            destTarget
              { otModules = mergedModules
              , otDependsOn = filteredDeps
              }
          updatedTarget =
            targetBase
              { otTargetName =
                  maybe destTarget.otTargetName (renderOutputTargetName destTarget.otDirectory) (targetPrimaryModule targetBase)
              }
          targets' =
            HM.insert destId updatedTarget (HM.delete sourceId state.ogTargets)
          moduleToTarget' =
            foldl'
              (\acc moduleName -> HM.insert moduleName updatedTarget.otTargetName acc)
              state.ogModuleToTarget
              (Set.toList sourceTarget.otModules)
          affectedTargets =
            Set.toList
              ( Set.insert
                  destId
                  (HM.lookupDefault Set.empty sourceId state.ogReverseTargetDeps)
              )
          targetDeps' =
            recomputeDependencies
              targets'
              moduleToTarget'
              state.ogTargetDeps
              sourceId
              affectedTargets
          reverseDeps' =
            recomputeReverseDependencies
              targetDeps'
              state.ogReverseTargetDeps
              state.ogTargetDeps
              sourceId
              affectedTargets
       in if createsCycle targetDeps' destId
            then Nothing
            else
              Just
                state
                  { ogTargets = targets'
                  , ogModuleToTarget = moduleToTarget'
                  , ogTargetDeps = targetDeps'
                  , ogReverseTargetDeps = reverseDeps'
                  }

outputToState :: BuildGraphOutput -> OutputGraphState
outputToState BuildGraphOutput {targets, moduleToTarget} =
  OutputGraphState
    { ogTargets = targetStates
    , ogModuleToTarget = moduleToTarget
    , ogTargetDeps = targetDeps
    , ogReverseTargetDeps = reverseDeps
    }
 where
  targetStates =
    HM.fromList
      [ (targetKeyId target.key, toState target)
      | target <- targets
      ]
  targetDeps = computeTargetDependencies targetStates moduleToTarget
  reverseDeps = invertDependencies targetDeps

toState :: TargetOutput -> OutputTargetState
toState TargetOutput {key, targetName, directory, modules, dependsOn} =
  OutputTargetState
    { otKey = key
    , otKeyId = targetKeyId key
    , otTargetName = targetName
    , otDirectory = directory
    , otModules = Set.fromList modules
    , otDependsOn = Set.fromList dependsOn
    }

stateToOutput :: OutputGraphState -> BuildGraphOutput
stateToOutput OutputGraphState {ogTargets, ogModuleToTarget} =
  BuildGraphOutput
    { targets =
        map
          toOutput
          (List.sortOn (\state -> state.otKeyId) (HM.elems ogTargets))
    , moduleToTarget = ogModuleToTarget
    }

toOutput :: OutputTargetState -> TargetOutput
toOutput OutputTargetState {otKey, otTargetName, otDirectory, otModules, otDependsOn} =
  TargetOutput
    { key = otKey
    , targetName = otTargetName
    , directory = otDirectory
    , modules = List.sort (Set.toList otModules)
    , dependsOn = List.sort (Set.toList otDependsOn)
    }

isModuleKey :: TargetKeyOutput -> Bool
isModuleKey key =
  case key of
    ModuleTargetOutput _ -> True
    _ -> False

targetKeyId :: TargetKeyOutput -> TargetNodeId
targetKeyId key =
  case key of
    DirectoryTargetOutput dir -> TargetNodeId ("directory:" <> dir)
    RecursiveDirectoryTargetOutput dir -> TargetNodeId ("recursive:" <> dir)
    ModuleTargetOutput name -> TargetNodeId ("module:" <> name)
    ExternalTargetOutput name -> TargetNodeId ("external:" <> name)

formatMergeResult :: Text -> TargetNodeId -> TargetNodeId -> Bool -> Int -> Int -> String
formatMergeResult dir destId sourceId succeeded attempt total =
  "GroupCandidates.merge: attempting merge in directory "
    ++ Text.unpack dir
    ++ " from "
    ++ Text.unpack (targetNodeIdText sourceId)
    ++ " into "
    ++ Text.unpack (targetNodeIdText destId)
    ++ " -> "
    ++ resultText
    ++ " ("
    ++ show attempt
    ++ "/"
    ++ show total
    ++ ", "
    ++ show (percentage attempt total)
    ++ "%)"
    ++ "\n"
 where
  resultText =
    if succeeded
      then "success"
      else "no merge"
  percentage _ tot | tot <= 0 = 100
  percentage att tot = (att * 100) `div` tot

renderOutputTargetName :: Text -> Text -> Text
renderOutputTargetName dir moduleName =
  "//" <> normalizeDirectory dir <> ":" <> moduleTargetSlug moduleName

normalizeDirectory :: Text -> Text
normalizeDirectory text =
  case cleaned of
    "" -> ""
    "." -> ""
    other -> stripLeadingSlashes other
 where
  cleaned = trimTrailingSlash text

stripLeadingSlashes :: Text -> Text
stripLeadingSlashes = Text.dropWhile (== '/')

trimTrailingSlash :: Text -> Text
trimTrailingSlash txt
  | Text.null txt = txt
  | otherwise = Text.dropWhileEnd (== '/') txt

moduleTargetSlug :: Text -> Text
moduleTargetSlug = Text.toLower . Text.replace "." "_"

recomputeDependencies ::
  HashMap TargetNodeId OutputTargetState ->
  HashMap Text Text ->
  HashMap TargetNodeId (Set TargetNodeId) ->
  TargetNodeId ->
  [TargetNodeId] ->
  HashMap TargetNodeId (Set TargetNodeId)
recomputeDependencies targets moduleToTarget oldDeps removedTarget affectedTargets =
  foldl'
    updateDeps
    (HM.delete removedTarget oldDeps)
    affectedTargets
 where
  nameIndex = targetNameIndex targets
  updateDeps acc targetId =
    case HM.lookup targetId targets of
      Nothing -> acc
      Just targetState ->
        HM.insert targetId (targetDependencies moduleToTarget nameIndex targetState) acc

recomputeReverseDependencies ::
  HashMap TargetNodeId (Set TargetNodeId) ->
  HashMap TargetNodeId (Set TargetNodeId) ->
  HashMap TargetNodeId (Set TargetNodeId) ->
  TargetNodeId ->
  [TargetNodeId] ->
  HashMap TargetNodeId (Set TargetNodeId)
recomputeReverseDependencies
  newDeps
  oldReverse
  oldDeps
  removedTarget
  affectedTargets =
    foldl'
      updateReverse
      cleanedReverse
      affectedTargets
   where
    sourceOldDeps = HM.lookupDefault Set.empty removedTarget oldDeps
    withoutRemovedEntry = HM.delete removedTarget oldReverse
    cleanedReverse =
      Set.foldl'
        (\acc dep -> HM.alter (removeDependent removedTarget) dep acc)
        withoutRemovedEntry
        sourceOldDeps

    updateReverse acc targetId =
      let oldDepsForTarget = HM.lookupDefault Set.empty targetId oldDeps
          newDepsForTarget = HM.lookupDefault Set.empty targetId newDeps
          removedDeps = Set.toList (Set.difference oldDepsForTarget newDepsForTarget)
          addedDeps = Set.toList (Set.difference newDepsForTarget oldDepsForTarget)
          accWithoutRemoved =
            foldl'
              (\innerAcc dep -> HM.alter (removeDependent targetId) dep innerAcc)
              acc
              removedDeps
       in foldl'
            (\innerAcc dep -> HM.insertWith Set.union dep (Set.singleton targetId) innerAcc)
            accWithoutRemoved
            addedDeps

removeDependent :: TargetNodeId -> Maybe (Set TargetNodeId) -> Maybe (Set TargetNodeId)
removeDependent target maybeSet =
  case maybeSet of
    Nothing -> Nothing
    Just entries ->
      let remaining = Set.delete target entries
       in if Set.null remaining then Nothing else Just remaining

createsCycle :: HashMap TargetNodeId (Set TargetNodeId) -> TargetNodeId -> Bool
createsCycle adjMap targetId =
  depthFirst Set.empty (Set.toList (HM.lookupDefault Set.empty targetId adjMap))
 where
  depthFirst _ [] = False
  depthFirst visited (node : rest)
    | node == targetId = True
    | Set.member node visited = depthFirst visited rest
    | otherwise =
        let neighbours = Set.toList (HM.lookupDefault Set.empty node adjMap)
         in depthFirst (Set.insert node visited) (neighbours ++ rest)

computeTargetDependencies ::
  HashMap TargetNodeId OutputTargetState ->
  HashMap Text Text ->
  HashMap TargetNodeId (Set TargetNodeId)
computeTargetDependencies targets moduleToTarget =
  let nameIndex = targetNameIndex targets
   in HM.map (targetDependencies moduleToTarget nameIndex) targets

targetDependencies ::
  HashMap Text Text ->
  HashMap Text TargetNodeId ->
  OutputTargetState ->
  Set TargetNodeId
targetDependencies moduleToTarget nameIndex target =
  Set.fromList
    [ depId
    | moduleName <- Set.toList target.otDependsOn
    , Just assignedName <- [HM.lookup moduleName moduleToTarget]
    , Just depId <- [HM.lookup assignedName nameIndex]
    , depId /= target.otKeyId
    ]

targetNameIndex :: HashMap TargetNodeId OutputTargetState -> HashMap Text TargetNodeId
targetNameIndex targets =
  HM.fromList
    [ (targetState.otTargetName, targetId)
    | (targetId, targetState) <- HM.toList targets
    ]

invertDependencies :: HashMap TargetNodeId (Set TargetNodeId) -> HashMap TargetNodeId (Set TargetNodeId)
invertDependencies deps =
  HM.foldlWithKey'
    ( \acc from toSet ->
        Set.foldl'
          (\innerAcc to -> HM.insertWith Set.union to (Set.singleton from) innerAcc)
          acc
          toSet
    )
    HM.empty
    deps

recomputeAllDependencies :: OutputGraphState -> OutputGraphState
recomputeAllDependencies state =
  state
    { ogTargetDeps = deps
    , ogReverseTargetDeps = invertDependencies deps
    }
 where
  deps = computeTargetDependencies state.ogTargets state.ogModuleToTarget
