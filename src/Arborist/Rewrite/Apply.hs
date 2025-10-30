{-# LANGUAGE OverloadedStrings #-}

module Arborist.Rewrite.Apply (
  writeEdit,
  writeMultipleEdits,
) where

import Arborist.Rewrite.Core (applyEdit, applyMultipleEdits)
import Data.Edit (Edit)
import Data.Text (Text)
import Data.Text.IO qualified as TIO

-- | Apply an edit to a file and rewrite it
writeEdit :: FilePath -> Edit -> IO ()
writeEdit filePath edit = do
  originalContent <- TIO.readFile filePath
  let newContent = applyEdit edit originalContent
  TIO.writeFile filePath newContent

-- | Apply multiple edits to a file, adjusting subsequent edit positions
-- based on previous changes. Edits are applied in order.
writeMultipleEdits :: FilePath -> [Edit] -> IO ()
writeMultipleEdits filePath edits = do
  originalContent <- TIO.readFile filePath
  let newContent = applyMultipleEdits edits originalContent
  TIO.writeFile filePath newContent
