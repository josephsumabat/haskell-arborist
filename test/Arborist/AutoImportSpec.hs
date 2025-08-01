{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Arborist.AutoImportSpec (spec) where

import Test.Hspec
import Hir.Parse qualified as Parse
import Arborist.AutoImport (addDeclToImportEdit)
import Data.Edit (getChanges)
import Data.Change (Change(..))
import TestImport (getPrg, getDecls)
import AST qualified
import AST.Haskell qualified as H
import Hir.Types
import Data.Either.Extra (fromRight)

spec :: Spec
spec = do
  describe "addDeclToImportEdit" $ do

    it "adds function to existing import list" $ do
      programs <- getPrg ["./test-data/auto-import/PartialImportExample.hs"]
      let [prog] = programs
          haskelProgram = prog.node
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode haskelProgram)
          testDecl = head (getDecls prog)

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport testDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import Data.List (map, sort)"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"

    it "adds class with (..) to empty import list" $ do
      importingPrograms <- getPrg ["./test-data/auto-import/ImportTestTypes.hs"]
      exportPrograms <- getPrg ["./test-data/auto-import/TestTypes.hs"]
      
      let [importingProg] = importingPrograms
          [exportProg] = exportPrograms
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode importingProg.node)
          classDecl = head (getDecls exportProg)

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport classDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import TestTypes (MyClass (..))"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"

    it "adds newtype with (..) to empty import list" $ do
      importingPrograms <- getPrg ["./test-data/auto-import/ImportTestTypes.hs"]
      exportPrograms <- getPrg ["./test-data/auto-import/TestTypes.hs"]
      
      let [importingProg] = importingPrograms
          [exportProg] = exportPrograms
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode importingProg.node)
          newtypeDecl = (getDecls exportProg) !! 1

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport newtypeDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import TestTypes (MyNewtype (..))"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"

    it "adds data type with (..) to empty import list" $ do
      importingPrograms <- getPrg ["./test-data/auto-import/ImportTestTypes.hs"]
      exportPrograms <- getPrg ["./test-data/auto-import/TestTypes.hs"]
      
      let [importingProg] = importingPrograms
          [exportProg] = exportPrograms
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode importingProg.node)
          dataDecl = (getDecls exportProg) !! 3

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport dataDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import TestTypes (MyData (..))"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"

    it "adds regular function to empty import list" $ do
      importingPrograms <- getPrg ["./test-data/auto-import/ImportTestTypes.hs"]
      exportPrograms <- getPrg ["./test-data/auto-import/TestTypes.hs"]
      
      let [importingProg] = importingPrograms
          [exportProg] = exportPrograms
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode importingProg.node)
          fooDecl = (getDecls exportProg) !! 4

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport fooDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import TestTypes (foo)"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"

    it "adds operator with parentheses to empty import list" $ do
      importingPrograms <- getPrg ["./test-data/auto-import/ImportTestTypes.hs"]
      exportPrograms <- getPrg ["./test-data/auto-import/TestTypes.hs"]
      
      let [importingProg] = importingPrograms
          [exportProg] = exportPrograms
          importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode importingProg.node)
          operatorDecl = (getDecls exportProg) !! 6

      case importNode of
        Just import' -> do
          let dynNode = AST.getDynNode import'
              hirImport =  fromRight (error "Failed to parse import") $ Parse.parseImport import'
              changes = getChanges (addDeclToImportEdit hirImport operatorDecl)
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "import TestTypes ((***))"
            _ -> expectationFailure $ show changes
        Nothing -> expectationFailure "No import found"
