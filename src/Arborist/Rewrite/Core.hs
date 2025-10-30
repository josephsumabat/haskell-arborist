{-# LANGUAGE OverloadedStrings #-}

module Arborist.Rewrite.Core (
  applyEdit,
  applyMultipleEdits,
  adjustEdit,
) where

import Data.Change (Change (..))
import Data.Edit (Edit)
import Data.Edit qualified as Edit
import Data.List (foldl', sortOn)
import Data.Ord qualified as Ord
import Data.Pos (Pos (..))
import Data.Range as Range (Range (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | Apply an edit to text content
applyEdit :: Edit -> Text -> Text
applyEdit edit originalText =
  let changes = Edit.getChanges edit
      -- Sort changes by position (highest to lowest) to avoid position shifts
      sortedChanges = sortOn (Ord.Down . (\c -> c.delete.start.pos)) changes
   in foldl' applyChange originalText sortedChanges
 where
  applyChange :: Text -> Change -> Text
  applyChange text change =
    let start = change.delete.start.pos
        end = change.delete.end.pos
        before = T.take start text
        after = T.drop end text
        inserted = change.insert
     in before <> inserted <> after

-- | Apply multiple edits to text content, adjusting positions for each edit
applyMultipleEdits :: [Edit] -> Text -> Text
applyMultipleEdits edits originalText =
  let (finalText, _) = foldl' applyEditWithAdjustment (originalText, []) edits
   in finalText

-- | Apply a single edit to text, returning the adjusted text and position adjustments
-- This tracks how each edit affects subsequent positions
applyEditWithAdjustment :: (Text, [(Pos, Int)]) -> Edit -> (Text, [(Pos, Int)])
applyEditWithAdjustment (text, adjustments) edit =
  let changes = Edit.getChanges edit
      -- Sort changes by position (highest to lowest) to avoid position shifts
      sortedChanges = sortOn (Ord.Down . (\c -> c.delete.start.pos)) changes
      (newText, newAdjustments) = foldl applyChangeWithAdjustment (text, adjustments) sortedChanges
   in (newText, newAdjustments)
 where
  applyChangeWithAdjustment :: (Text, [(Pos, Int)]) -> Change -> (Text, [(Pos, Int)])
  applyChangeWithAdjustment (text, adjustments) change =
    let start = adjustPosition change.delete.start adjustments
        end = adjustPosition change.delete.end adjustments
        before = T.take start text
        after = T.drop end text
        inserted = change.insert
        newText = before <> inserted <> after
        -- Calculate how this change affects subsequent positions
        deletedLength = end - start
        insertedLength = T.length inserted
        positionShift = insertedLength - deletedLength
        newAdjustments = (change.delete.start, positionShift) : adjustments
     in (newText, newAdjustments)

-- | Adjust a position based on previous edits
adjustPosition :: Pos -> [(Pos, Int)] -> Int
adjustPosition pos adjustments =
  let relevantAdjustments = filter (\(adjustPos, _) -> adjustPos.pos <= pos.pos) adjustments
      totalShift = sum [shift | (_, shift) <- relevantAdjustments]
   in pos.pos + totalShift

-- | Create an adjusted edit based on previous position adjustments
-- This is useful when you need to create new edits after previous ones have been applied
adjustEdit :: Edit -> [(Pos, Int)] -> Edit
adjustEdit edit adjustments =
  let changes = Edit.getChanges edit
      adjustedChanges = map (adjustChange adjustments) changes
   in Edit.changesToEdit adjustedChanges
 where
  adjustChange :: [(Pos, Int)] -> Change -> Change
  adjustChange adjustments change =
    let adjustedStart = change.delete.start {pos = adjustPosition change.delete.start adjustments}
        adjustedEnd = change.delete.end {pos = adjustPosition change.delete.end adjustments}
     in change {delete = change.delete {Range.start = adjustedStart, end = adjustedEnd}}
