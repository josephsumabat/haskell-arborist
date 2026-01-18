{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DynNode.Snapshot
  ( DynNodeId (..)
  , DynNodeSnapshot (..)
  , SnapshotHeap (..)
  , SnapshotArchive (..)
  , SnapshotError (..)
  , snapshotDynNode
  , snapshotArchive
  , restoreDynNode
  , restoreSnapshotArchive
  )
where

import AST (DynNode)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Fix (mfix)
import Data.Dynamic (Dynamic)
import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.LineColRange (LineColRange)
import Data.Range (Range)
import Data.Text (Text)
import Data.List (foldl')
import Data.Word (Word64)
import TreeSitter.Api qualified as TS

data DynNodeId = DynNodeId
  { dynNodeHash :: !Word64
  , dynNodePath :: ![Int]
  }
  deriving stock (Show)

instance Eq DynNodeId where
  (==) = (==) `on` (.dynNodePath)

instance Ord DynNodeId where
  compare = compare `on` (.dynNodePath)

instance Hashable DynNodeId where
  hashWithSalt salt = hashWithSalt salt . (.dynNodePath)

data DynNodeSnapshot = DynNodeSnapshot
  { snapshotType :: !Text
  , snapshotSymbol :: !TS.Symbol
  , snapshotRange :: !Range
  , snapshotLineColRange :: !LineColRange
  , snapshotFieldName :: !(Maybe Text)
  , snapshotIsNamed :: !Bool
  , snapshotIsExtra :: !Bool
  , snapshotText :: !Text
  , snapshotParent :: !(Maybe DynNodeId)
  , snapshotChildren :: ![DynNodeId]
  , snapshotExt :: !(Maybe Dynamic)
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

snapshotDynNode :: DynNode -> (SnapshotHeap, DynNodeId)
snapshotDynNode root =
  let (nodeId, acc) = snapshotNode root rootPath Nothing
   in (SnapshotHeap acc, nodeId)
  where
    rootPath = NodePath []

snapshotArchive :: DynNode -> SnapshotArchive
snapshotArchive root =
  let (heap', rootId') = snapshotDynNode root
   in SnapshotArchive {rootId = rootId', heap = heap'}

restoreDynNode :: SnapshotHeap -> DynNodeId -> Either SnapshotError DynNode
restoreDynNode heap' root = runExcept $ go Nothing root
  where
  go :: Maybe DynNode -> DynNodeId -> Except SnapshotError DynNode
  go parent nodeId = do
    snapshot <- lookupSnapshot nodeId
    mfix $ \node -> do
      children <- traverse (go (Just node)) snapshot.snapshotChildren
      pure
        TS.Node
          { nodeType = snapshot.snapshotType
          , nodeSymbol = snapshot.snapshotSymbol
          , nodeRange = snapshot.snapshotRange
          , nodeLineColRange = snapshot.snapshotLineColRange
          , nodeFieldName = snapshot.snapshotFieldName
          , nodeIsNamed = snapshot.snapshotIsNamed
          , nodeIsExtra = snapshot.snapshotIsExtra
          , nodeText = snapshot.snapshotText
          , nodeChildren = children
          , nodeParent = parent
          , nodeExt = snapshot.snapshotExt
          }

  lookupSnapshot :: DynNodeId -> Except SnapshotError DynNodeSnapshot
  lookupSnapshot nodeId =
    case HashMap.lookup nodeId heap'.snapshots of
      Nothing -> throwError (MissingSnapshot nodeId)
      Just snapshot -> pure snapshot

restoreSnapshotArchive :: SnapshotArchive -> Either SnapshotError DynNode
restoreSnapshotArchive archive = restoreDynNode archive.heap archive.rootId

snapshotNode :: DynNode -> NodePath -> Maybe DynNodeId -> (DynNodeId, HashMap DynNodeId DynNodeSnapshot)
snapshotNode node path parentId =
  let nodeId = mkDynNodeId node pathList
      childResults =
        zipWith
          (\child idx -> snapshotNode child (extendPath path idx) (Just nodeId))
          node.nodeChildren
          [0 ..]
      childIds = fmap fst childResults
      childMaps = fmap snd childResults
      snapshot =
        DynNodeSnapshot
          { snapshotType = node.nodeType
          , snapshotSymbol = node.nodeSymbol
          , snapshotRange = node.nodeRange
          , snapshotLineColRange = node.nodeLineColRange
          , snapshotFieldName = node.nodeFieldName
          , snapshotIsNamed = node.nodeIsNamed
          , snapshotIsExtra = node.nodeIsExtra
          , snapshotText = node.nodeText
          , snapshotParent = parentId
          , snapshotChildren = childIds
          , snapshotExt = node.nodeExt
          }
      currentMap = HashMap.singleton nodeId snapshot
      combinedMap = HashMap.unions (currentMap : childMaps)
   in (nodeId, combinedMap)
  where
    pathList = finalizePath path

newtype NodePath = NodePath [Int]

extendPath :: NodePath -> Int -> NodePath
extendPath (NodePath components) idx = NodePath (idx : components)

finalizePath :: NodePath -> [Int]
finalizePath (NodePath components) = reverse components

mkDynNodeId :: DynNode -> [Int] -> DynNodeId
mkDynNodeId node pathList =
  let hashSteps :: [Int -> Int]
      hashSteps =
        [ (`hashWithSalt` node.nodeType)
        , (`hashWithSalt` node.nodeText)
        , (`hashWithSalt` node.nodeFieldName)
        , (`hashWithSalt` node.nodeIsNamed)
        , (`hashWithSalt` node.nodeIsExtra)
        , (`hashWithSalt` show node.nodeRange)
        , (`hashWithSalt` show node.nodeLineColRange)
        , (`hashWithSalt` node.nodeSymbol.symbolId)
        , (`hashWithSalt` pathList)
        ]
      hashed' = foldl' (\acc f -> f acc) 0 hashSteps
   in DynNodeId
        { dynNodeHash = fromIntegral hashed'
        , dynNodePath = pathList
        }
