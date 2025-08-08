{-# LANGUAGE OverloadedStrings #-}

module Arborist.Rewrite (
  rewriteNode,
  replaceRange,
  applyEdit,
  applySourceEdit,
  adjustEdit,
  applyMultipleEdits,
) where

import AST (DynNode)
import AST qualified
import Data.Change (Change (..))
import Data.Edit
import Data.Edit qualified as Edit
import Data.LineCol
import Data.LineColRange
import Data.List (foldl')
import Data.Ord qualified as Ord
import Data.Pos
import Data.Range as Range (Range (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.HashMap.Strict qualified as HashMap
import Data.SourceEdit (SourceEdit(..), FsEdit(..))
import Data.Path qualified as Path
import System.FilePath qualified as FilePath
import System.Directory qualified as Dir
import Arborist.Rewrite.Apply qualified as Apply
import Arborist.Rewrite.Core (applyEdit, applyMultipleEdits, adjustEdit)

replaceRange :: LineColRange -> Text -> Text -> Text
replaceRange (LineColRange (LineCol startLine startCol) (LineCol endLine endCol)) replacement original =
  let ls = T.lines original
      before = take (startLine.pos - 1) ls
      targetLines = take (endLine.pos - startLine.pos + 1) $ drop (startLine.pos - 1) ls
      after = drop endLine.pos ls

      -- Split first and last line of the target range
      firstLinePrefix = T.take (startCol.pos - 1) (head targetLines)
      lastLineSuffix = T.drop (endCol.pos - 1) (last targetLines)

      -- Build the new target line(s)
      newTarget = case targetLines of
        [_singleLine] -> firstLinePrefix <> replacement <> lastLineSuffix
        _ ->
          firstLinePrefix <> replacement <> lastLineSuffix

      -- Reconstruct the full text
      resultLines = before ++ [newTarget] ++ after
   in T.unlines resultLines

rewriteNode :: DynNode -> Text -> Edit
rewriteNode dynNode newText =
  let range = dynNode.nodeRange
   in replace range newText

-- | Apply an edit to text content
-- re-exported via Core
-- applyEdit, applyMultipleEdits come from Core

-- | Apply a SourceEdit: write file edits and perform filesystem edits (moves)
applySourceEdit :: SourceEdit -> IO ()
applySourceEdit SourceEdit{fileEdits, fsEdits} = do
  -- Apply per-file edits
  mapM_
    (\(absPath, edit) -> do
        let fp = Path.toFilePath absPath
        Apply.writeEdit fp edit
    )
    (HashMap.toList fileEdits)
  -- Apply filesystem edits (e.g., move files)
  mapM_ applyFsEdit fsEdits
 where
  applyFsEdit :: FsEdit -> IO ()
  applyFsEdit (FsEditMoveFile{src, dst}) = do
    let srcFp = Path.toFilePath src
        dstFp = Path.toFilePath dst
        dstDir = FilePath.takeDirectory dstFp
    Dir.createDirectoryIfMissing True dstDir
    Dir.renamePath srcFp dstFp

-- | Apply a single edit to text, returning the adjusted text and position adjustments
-- This tracks how each edit affects subsequent positions
-- adjustEdit is imported from Core
