{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DynNode.Snapshot
  ( DynNodeId (..)
  , DynNodeSnapshot (..)
  , SnapshotHeap (..)
  , SnapshotArchive (..)
  , SnapshotError (..)
  , snapshotDynNode
  , snapshotTree
  , restoreDynNode
  , restoreSnapshotArchive
  , nodeId
  )
where

import AST (DynNode)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Fix (mfix)
import Data.Dynamic (Dynamic)
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.LineColRange (LineColRange)
import Data.Range (Range)
import Data.Text (Text)
import Data.List (foldl')
import Data.Word (Word64)
import TreeSitter.Api qualified as TS

newtype DynNodeId = DynNodeId {unDynNodeId :: Word64}
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

data DynNodeSnapshot = DynNodeSnapshot
  { nodeId :: !DynNodeId
  , nodeType :: !Text
  , nodeSymbol :: !TS.Symbol
  , nodeRange :: !Range
  , nodeLineColRange :: !LineColRange
  , nodeFieldName :: !(Maybe Text)
  , nodeIsNamed :: !Bool
  , nodeIsExtra :: !Bool
  , nodeText :: !Text
  , nodeParent :: !(Maybe DynNodeId)
  , nodeChildren :: ![DynNodeId]
  , nodeExt :: !(Maybe Dynamic)
  }

newtype SnapshotHeap = SnapshotHeap
  { snapshots :: HashMap DynNodeId DynNodeSnapshot
  }

data SnapshotArchive = SnapshotArchive
  { rootId :: DynNodeId
  , heap :: SnapshotHeap
  }

data SnapshotError
  = MissingSnapshot DynNodeId
  deriving stock (Eq, Show)

snapshotDynNode :: DynNode -> DynNodeSnapshot
snapshotDynNode node =
  let thisId = nodeId node
   in DynNodeSnapshot
        { nodeId = thisId
        , nodeType = node.nodeType
        , nodeSymbol = node.nodeSymbol
        , nodeRange = node.nodeRange
        , nodeLineColRange = node.nodeLineColRange
        , nodeFieldName = node.nodeFieldName
        , nodeIsNamed = node.nodeIsNamed
        , nodeIsExtra = node.nodeIsExtra
        , nodeText = node.nodeText
        , nodeParent = nodeId <$> node.nodeParent
        , nodeChildren = nodeId <$> node.nodeChildren
        , nodeExt = node.nodeExt
        }

snapshotTree :: DynNode -> SnapshotArchive
snapshotTree root =
  let rootSnapshotId = nodeId root
      heapMap = snapshotSubtree root
   in SnapshotArchive {rootId = rootSnapshotId, heap = SnapshotHeap heapMap}

restoreDynNode :: SnapshotHeap -> DynNodeId -> Either SnapshotError DynNode
restoreDynNode heap' root = runExcept $ go Nothing root
  where
  go :: Maybe DynNode -> DynNodeId -> Except SnapshotError DynNode
  go parent snapshotId = do
    snapshot <- lookupSnapshot snapshotId
    mfix $ \node -> do
      children <- traverse (go (Just node)) snapshot.nodeChildren
      pure
        TS.Node
          { nodeType = snapshot.nodeType
          , nodeSymbol = snapshot.nodeSymbol
          , nodeRange = snapshot.nodeRange
          , nodeLineColRange = snapshot.nodeLineColRange
          , nodeFieldName = snapshot.nodeFieldName
          , nodeIsNamed = snapshot.nodeIsNamed
          , nodeIsExtra = snapshot.nodeIsExtra
          , nodeText = snapshot.nodeText
          , nodeChildren = children
          , nodeParent = parent
          , nodeExt = snapshot.nodeExt
          }

  lookupSnapshot :: DynNodeId -> Except SnapshotError DynNodeSnapshot
  lookupSnapshot nodeId =
    case HashMap.lookup nodeId heap'.snapshots of
      Nothing -> throwError (MissingSnapshot nodeId)
      Just snapshot -> pure snapshot

restoreSnapshotArchive :: SnapshotArchive -> Either SnapshotError DynNode
restoreSnapshotArchive archive = restoreDynNode archive.heap archive.rootId

snapshotSubtree :: DynNode -> HashMap DynNodeId DynNodeSnapshot
snapshotSubtree node =
  let nodeSnapshotId = nodeId node
      nodeSnapshot = snapshotDynNode node
      childMaps = snapshotSubtree <$> node.nodeChildren
      currentMap = HashMap.singleton nodeSnapshotId nodeSnapshot
   in HashMap.unions (currentMap : childMaps)

nodeId :: DynNode -> DynNodeId
nodeId node = DynNodeId (fromIntegral hashed')
  where
    hashSteps :: [Int -> Int]
    hashSteps =
      [ (`hashWithSalt` node.nodeType)
      , (`hashWithSalt` node.nodeText)
      , (`hashWithSalt` node.nodeFieldName)
      , (`hashWithSalt` node.nodeIsNamed)
      , (`hashWithSalt` node.nodeIsExtra)
      , (`hashWithSalt` show node.nodeRange)
      , (`hashWithSalt` show node.nodeLineColRange)
      , (`hashWithSalt` node.nodeSymbol.symbolId)
      ]
    hashed' = foldl' (\acc f -> f acc) 0 hashSteps
