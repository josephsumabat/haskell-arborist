{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Arborist.Refactor.ModuleSpec (spec) where

import Test.Hspec
import Data.HashMap.Strict qualified as HashMap
import Data.Edit (getChanges)
import Data.Change (Change(..))
import Data.Path qualified as Path
import Data.SourceEdit (FsEdit (..), SourceEdit (..))
import Arborist.Refactor.Module (renameModule)
import Hir.Parse (parseModuleTextFromText)
import TestImport (lazyGetPrgs)

spec :: Spec
spec = do
  describe "renameModule" $ do
    it "rewrites header and emits a move (even with no importers)" $ do
      prgs <- lazyGetPrgs [ "./test-data/auto-export/EmptyHeaderExample.hs" ]
      let oldMod = parseModuleTextFromText "StaticLS.IDE.SourceEdit"
          newMod = parseModuleTextFromText "StaticLS.IDE.Refactored.SourceEdit"
          se@SourceEdit{fileEdits, fsEdits} = renameModule prgs oldMod newMod

      -- It should include at least one file edit and a move
      HashMap.size fileEdits `shouldSatisfy` (> 0)

      -- Expect exact inserted text in the edit
      case HashMap.toList fileEdits of
        [(p, e)] -> do
          Path.toFilePath p `shouldBe` "StaticLS/IDE/SourceEdit.hs"
          case getChanges e of
            [Change inserted _] -> inserted `shouldBe` "StaticLS.IDE.Refactored.SourceEdit"
            other -> expectationFailure ("unexpected edit changes: " <> show other)
        other -> expectationFailure ("unexpected fileEdits: " <> show other)

      case fsEdits of
        [FsEditMoveFile{src, dst}] -> do
          Path.toFilePath src `shouldBe` "StaticLS/IDE/SourceEdit.hs"
          Path.toFilePath dst `shouldBe` "StaticLS/IDE/Refactored/SourceEdit.hs"
        other -> expectationFailure ("unexpected fsEdits: " <> show other)
