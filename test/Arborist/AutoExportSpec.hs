{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Arborist.AutoExportSpec (spec) where

import Test.Hspec
import Hir.Parse       
import Arborist.AutoExport (getAllDeclExportEdit, getDeclExportEdit)
import Data.Edit (getChanges)
import Data.Change (Change(..))
import TestImport (getPrg, getDecls)
import AST qualified
import AST.Haskell qualified as H
import Hir.Types

spec :: Spec
spec = do
  describe "getAllDeclExportEdit" $ do

    it "getAllDeclExportEdit: inserts all exports to empty export list" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getAllDeclExportEdit prog progH) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getAllDeclExportEdit: inserts all exports to export list w/ modules already present" $ do
      programs <- getPrg ["./test-data/auto-export/HalfFullExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getAllDeclExportEdit prog progH) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getAllDeclExportEdit: inserts all exports to export list w/ all modules already present" $ do
      programs <- getPrg ["./test-data/auto-export/FullExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getAllDeclExportEdit prog progH) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"
  
    it "getAllDeclExportEdit: inserts all exports to an empty header when only one decl exists" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyHeaderOneDeclExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getAllDeclExportEdit prog progH) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "module StaticLS.IDE.SourceEdit (foo) where"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getAllDeclExportEdit: inserts all exports to an empty header" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyHeaderExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getAllDeclExportEdit prog progH) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "module StaticLS.IDE.SourceEdit (foo, bar) where"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getDeclExportEdit: adds a single export to an empty export list" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyExample.hs"]
      let [prog] = programs
          newDecl = (getDecls prog) !! 1
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getDeclExportEdit progH newDecl) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "(foo)"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getDeclExportEdit: appends a single export to a partially filled list" $ do
      programs <- getPrg ["./test-data/auto-export/HalfFullExample.hs"]
      let [prog] = programs
          newDecl = (getDecls prog) !! 3
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getDeclExportEdit progH newDecl) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "(foo, bar)"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"

    it "getDeclExportEdit: appends a single export to an empty header" $ do
      programs <- getPrg ["./test-data/auto-export/EmptyHeaderExample.hs"]
      let [prog] = programs
          newDecl = (getDecls prog) !! 1
          haskelProgram = prog.node
          progHeader = findNode (AST.cast @H.HeaderP) (AST.getDynNode haskelProgram)

      case progHeader of 
        Just progH -> 
                    let changes = getChanges (getDeclExportEdit progH newDecl) 
                    in 
                    case changes of
                      [Change insertedText _] -> insertedText `shouldBe` "module StaticLS.IDE.SourceEdit (foo) where"
                      _ ->  expectationFailure $ show changes
        _ -> expectationFailure "not a program"
