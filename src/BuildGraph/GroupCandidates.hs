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
import Debug.Trace (trace)

newtype TargetNodeId = TargetNodeId Text
  deriving (Eq, Ord, Show, Hashable)

targetNodeIdText :: TargetNodeId -> Text
targetNodeIdText (TargetNodeId txt) = txt

data OutputGraphState = OutputGraphState
  { ogTargets :: !(HashMap TargetNodeId OutputTargetState)
  , ogModuleToTarget :: !(HashMap Text TargetKeyOutput)
  , ogTargetDeps :: !(HashMap TargetNodeId (Set TargetNodeId))
  , ogReverseTargetDeps :: !(HashMap TargetNodeId (Set TargetNodeId))
  }

data OutputTargetState = OutputTargetState
  { otKey :: !TargetKeyOutput
  , otKeyId :: !TargetNodeId
  , otDirectory :: !Text
  , otModules :: !(Set Text)
  , otDependsOn :: !(Set Text)
  }

groupOutputCandidates :: BuildGraphOutput -> [DirName] -> BuildGraphOutput
groupOutputCandidates output requestedDirs =
  stateToOutput finalState
 where
  requestedSet =
    case requestedDirs of
      [] -> Nothing
      _ -> Just (Set.fromList (map dirNameText requestedDirs))
  initialState = outputToState output
  availableDirs = collectGroupableDirectories initialState
  targetDirs =
    maybe availableDirs (`Set.intersection` availableDirs) requestedSet
  finalState =
    foldl' mergeDir initialState (List.sort (Set.toList targetDirs))

  dirNameText (DirName txt) = txt

mergeDir :: OutputGraphState -> Text -> OutputGraphState
mergeDir state dir =
  finalState
 where
  moduleIds =
    List.sort
      [ target.otKeyId
      | target <- HM.elems state.ogTargets
      , target.otDirectory == dir
      , isModuleKey target.otKey
      ]
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

collectGroupableDirectories :: OutputGraphState -> Set Text
collectGroupableDirectories state =
  Set.fromList
    [ target.otDirectory
    | target <- HM.elems state.ogTargets
    , isModuleKey target.otKey
    ]

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
          updatedTarget =
            destTarget
              { otModules = mergedModules
              , otDependsOn = filteredDeps
              }
          targets' =
            HM.insert destId updatedTarget (HM.delete sourceId state.ogTargets)
          moduleToTarget' =
            foldl'
              (\acc moduleName -> HM.insert moduleName destTarget.otKey acc)
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
toState TargetOutput {key, directory, modules, dependsOn} =
  OutputTargetState
    { otKey = key
    , otKeyId = targetKeyId key
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
toOutput OutputTargetState {otKey, otDirectory, otModules, otDependsOn} =
  TargetOutput
    { key = otKey
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

recomputeDependencies ::
  HashMap TargetNodeId OutputTargetState ->
  HashMap Text TargetKeyOutput ->
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
  updateDeps acc targetId =
    case HM.lookup targetId targets of
      Nothing -> acc
      Just targetState ->
        HM.insert targetId (targetDependencies moduleToTarget targetState) acc

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
  HashMap Text TargetKeyOutput ->
  HashMap TargetNodeId (Set TargetNodeId)
computeTargetDependencies targets moduleToTarget =
  HM.map (targetDependencies moduleToTarget) targets

targetDependencies :: HashMap Text TargetKeyOutput -> OutputTargetState -> Set TargetNodeId
targetDependencies moduleToTarget target =
  Set.fromList
    [ targetKeyId depKey
    | moduleName <- Set.toList target.otDependsOn
    , Just depKey <- [HM.lookup moduleName moduleToTarget]
    , let depId = targetKeyId depKey
    , depId /= target.otKeyId
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
