{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Arborist.AutoExportSpec (spec) where

import Test.Hspec
import Hir.Parse       
import Arborist.AutoExport (getAllDeclExportEdits, getDeclExportEdits)
import Data.Edit (empty, getChanges)
import Data.Change (Change(..))
import TestImport (getPrg, getDecls)

spec :: Spec
spec = do
  describe "getAllDeclExportEdits" $ do

    it "does nothing on an empty program" $
      getAllDeclExportEdits emptyProgram `shouldBe` empty

    it "getAllDeclExportEdits: inserts all exports to empty export list" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyExample.hs"]
      let [prog] = programs

      let changes = getChanges (getAllDeclExportEdits prog)
      case changes of
        [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
        _ ->  expectationFailure $ show changes

    it "getAllDeclExportEdits: inserts all exports to export list w/ modules already present" $ do
      programs <- getPrg ["./test-data/auto-export/HalfFullExample.hs"]
      let [prog] = programs

      let changes = getChanges (getAllDeclExportEdits prog)
      case changes of
        [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
        _ ->  expectationFailure $ show changes

    it "getAllDeclExportEdits: inserts all exports to export list all modules already present" $ do
      programs <- getPrg ["./test-data/auto-export/FullExample.hs"]
      let [prog] = programs

      let changes = getChanges (getAllDeclExportEdits prog)
      case changes of
        [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
        _ ->  expectationFailure $ show changes
    
    it "getDeclExportEdits: adds a single export to an empty export list" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyExample.hs"]
      let [prog] = programs
          newDecl = (getDecls prog) !! 1
      let changes = getChanges (getDeclExportEdits prog newDecl)
      case changes of
        [Change insertedText _] -> insertedText `shouldBe` "(foo)"
        _ ->  expectationFailure $ show changes

    it "getDeclExportEdits: appends a single export to a partially filled list" $ do
      programs <- getPrg ["./test-data/auto-export/HalfFullExample.hs"]
      let [prog] = programs
          newDecl = (getDecls prog) !! 3
      let changes = getChanges (getDeclExportEdits prog newDecl)
      case changes of
        [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
        _ ->  expectationFailure $ show changes
