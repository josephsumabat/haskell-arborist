{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Arborist.AutoImportSpec (spec) where

import Test.Hspec
import Hir.Parse
import Arborist.AutoImport (addDeclToImportEdit)
import Data.Edit (getChanges)
import Data.Change (Change(..))
import TestImport (getPrg, getDecls)
import AST qualified
import AST.Haskell qualified as H
import Hir.Types

spec :: Spec
spec = do
  describe "addDeclToImportEdit" $ do

    it "adds function to existing import list" $ do
      programs <- getPrg ["./test-data/auto-import/PartialImportExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          importNode = findNode (AST.cast @H.ImportP) (AST.getDynNode haskelProgram)
          testDecl = head (getDecls prog)

      case importNode of
        Just import' -> do
          let changes = getChanges (addDeclToImportEdit import' testDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import Data.List (map, sort)"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"