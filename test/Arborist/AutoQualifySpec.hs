{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Arborist.AutoQualifySpec (spec) where

import AST qualified
import AST.Haskell qualified as H
import Arborist.AutoQualify (qualifyIdentifier)
import Data.Change (Change (..))
import Data.Edit (getChanges)
import Data.Either.Extra (fromRight)
import Hir.Parse qualified as Parse
import Hir.Types
import Test.Hspec
import TestImport (getPrg)

spec :: Spec
spec = do
  describe "qualifyDecl" $ do
    it "qualifies foo with module alias" $ do
      programs <- getPrg ["./test-data/auto-qualify/QualifyFooWithAlias.hs"]
      let [prog] = programs
      let importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode prog.node)
      let fooDynNode = Parse.findNode (\n -> if AST.nodeToText n == "foo" then Just n else Nothing) (AST.getDynNode prog.node)
          fooASTNode = (AST.cast @H.VariableP) =<< fooDynNode
          fooNode = fmap (Parse.parseName . AST.Inj) fooASTNode
      case (importNode, fooNode) of
        (Just import', Just node) -> do
          let hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              edit = qualifyIdentifier node hirImport

              changes = getChanges edit
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "T.foo"
            _ -> expectationFailure $ show changes
        _ -> expectationFailure "Import or node not found"

    it "qualifies foo with qualified mpdule" $ do
      programs <- getPrg ["./test-data/auto-qualify/QualifyFooWithNoAlias.hs"]
      let [prog] = programs
      let importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode prog.node)
      let fooDynNode = Parse.findNode (\n -> if AST.nodeToText n == "foo" then Just n else Nothing) (AST.getDynNode prog.node)
          fooASTNode = (AST.cast @H.VariableP) =<< fooDynNode
          fooNode = fmap (Parse.parseName . AST.Inj) fooASTNode

      case (importNode, fooNode) of
        (Just import', Just node) -> do
          let hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              edit = qualifyIdentifier node hirImport
              changes = getChanges edit
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "TestTypes.foo"
            _ -> expectationFailure $ show changes
        _ -> expectationFailure "Import or node not found"
