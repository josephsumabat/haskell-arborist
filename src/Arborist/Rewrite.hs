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
import Arborist.Rewrite.Apply qualified as Apply
import Arborist.Rewrite.Core (adjustEdit, applyEdit, applyMultipleEdits)
import Control.Exception qualified as E
import Data.Change (Change (..))
import Data.Edit
import Data.Edit qualified as Edit
import Data.HashMap.Strict qualified as HashMap
import Data.LineCol
import Data.LineColRange
import Data.List (foldl')
import Data.Ord qualified as Ord
import Data.Path qualified as Path
import Data.Pos
import Data.Range as Range (Range (..))
import Data.SourceEdit (FsEdit (..), SourceEdit (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath
import System.IO qualified as IO

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
applySourceEdit SourceEdit {fileEdits, fsEdits} = do
  -- Apply per-file edits
  mapM_
    ( \(absPath, edit) -> do
        let fp = Path.toFilePath absPath
        res <- E.try (Apply.writeEdit fp edit) :: IO (Either E.SomeException ())
        case res of
          Left ex -> IO.hPutStrLn IO.stderr ("Warning: failed to apply edit to " <> fp <> ": " <> E.displayException ex)
          Right _ -> pure ()
    )
    (HashMap.toList fileEdits)
  -- Apply filesystem edits (e.g., move files)
  mapM_ applyFsEdit fsEdits
 where
  applyFsEdit :: FsEdit -> IO ()
  applyFsEdit (FsEditMoveFile {src, dst}) = do
    let srcFp = Path.toFilePath src
        dstFp = Path.toFilePath dst
        dstDir = FilePath.takeDirectory dstFp
    -- Ensure destination directory exists; warn on failure and continue
    resMkdir <- E.try (Dir.createDirectoryIfMissing True dstDir) :: IO (Either E.SomeException ())
    case resMkdir of
      Left ex -> IO.hPutStrLn IO.stderr ("Warning: failed to create directory " <> dstDir <> ": " <> E.displayException ex)
      Right _ -> pure ()
    -- Attempt the move; warn on failure and continue
    resRename <- E.try (Dir.renamePath srcFp dstFp) :: IO (Either E.SomeException ())
    case resRename of
      Left ex -> IO.hPutStrLn IO.stderr ("Warning: failed to move file from " <> srcFp <> " to " <> dstFp <> ": " <> E.displayException ex)
      Right _ -> pure ()

-- | Apply a single edit to text, returning the adjusted text and position adjustments
-- This tracks how each edit affects subsequent positions
-- adjustEdit is imported from Core
