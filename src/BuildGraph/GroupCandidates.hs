{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module BuildGraph.GroupCandidates (groupOutputCandidates) where

import BuildGraph.Directory
  ( BuildGraphOutput (..)
  , DirName (..)
  , TargetOutput (..)
  , TargetKeyOutput (..)
  )
import Data.Graph (SCC (..), stronglyConnComp)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (trace)

data OutputGraphState = OutputGraphState
  { ogTargets :: !(HashMap Text OutputTargetState)
  , ogModuleToTarget :: !(HashMap Text TargetKeyOutput)
  }

data OutputTargetState = OutputTargetState
  { otKey :: !TargetKeyOutput
  , otKeyId :: !Text
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
            (\(st, acc, count) sourceId ->
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

mergeModuleTargets :: OutputGraphState -> Text -> Text -> Text -> Maybe OutputGraphState
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
          newState =
            state
              { ogTargets = targets'
              , ogModuleToTarget = moduleToTarget'
              }
       in if introducesCycle newState
            then Nothing
            else Just newState

introducesCycle :: OutputGraphState -> Bool
introducesCycle state =
  any isCycle (stronglyConnComp nodes)
 where
  deps = computeTargetDependencies state
  nodes =
    [ (keyId, keyId, Set.toList toKeys)
    | (keyId, toKeys) <- HM.toList deps
    ]
  isCycle (CyclicSCC _) = True
  isCycle _ = False

computeTargetDependencies :: OutputGraphState -> HashMap Text (Set Text)
computeTargetDependencies OutputGraphState {ogTargets, ogModuleToTarget} =
  HM.map dependencyTargets ogTargets
 where
  dependencyTargets target =
    Set.fromList
      [ targetKeyId depKey
      | moduleName <- Set.toList target.otDependsOn
      , Just depKey <- [HM.lookup moduleName ogModuleToTarget]
      , let depId = targetKeyId depKey
      , depId /= target.otKeyId
      ]

outputToState :: BuildGraphOutput -> OutputGraphState
outputToState BuildGraphOutput {targets, moduleToTarget} =
  OutputGraphState
    { ogTargets =
        HM.fromList
          [ (targetKeyId target.key, toState target)
          | target <- targets
          ]
    , ogModuleToTarget = moduleToTarget
    }

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
        map toOutput
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

targetKeyId :: TargetKeyOutput -> Text
targetKeyId key =
  case key of
    DirectoryTargetOutput dir -> "directory:" <> dir
    RecursiveDirectoryTargetOutput dir -> "recursive:" <> dir
    ModuleTargetOutput name -> "module:" <> name

formatMergeResult :: Text -> Text -> Text -> Bool -> Int -> Int -> String
formatMergeResult dir destId sourceId succeeded attempt total =
  "GroupCandidates.merge: attempting merge in directory "
    ++ Text.unpack dir
    ++ " from "
    ++ Text.unpack sourceId
    ++ " into "
    ++ Text.unpack destId
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
