{-# LANGUAGE OverloadedStrings #-}

module Arborist.Rewrite where

import Data.LineCol
import Data.LineColRange
import Data.Pos
import Data.Text (Text)
import Data.Text qualified as T

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
