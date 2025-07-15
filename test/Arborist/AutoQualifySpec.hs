{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Arborist.AutoQualifySpec (spec) where

import Test.Hspec
import Hir.Parse qualified as Parse
import Arborist.AutoQualify (qualifyDeclUsage)
import Data.Edit (getChanges)
import Data.Change (Change(..))
import TestImport (getPrg)
import AST qualified
import AST.Haskell qualified as H
import Hir.Types
import Data.Either.Extra (fromRight)

spec :: Spec
spec = do
  describe "qualifyDeclUsage" $ do

    it "qualifies foo with module alias" $ do
      programs <- getPrg ["./test-data/auto-qualify/QualifyFooWithAlias.hs"]
      let [prog] = programs
      let importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode prog.node)
      let fooUsageNode = Parse.findNode (\n -> if AST.nodeToText n == "foo" then Just n else Nothing) (AST.getDynNode prog.node)
      
      case (importNode, fooUsageNode) of
        (Just import', Just usageNode) -> do
          let hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              edit = qualifyDeclUsage usageNode hirImport
              changes = getChanges edit
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "T.foo"
            _ -> expectationFailure $ show changes
            
        _ -> expectationFailure "Import or usage not found"

    it "qualifies foo with qualified mpdule" $ do
      programs <- getPrg ["./test-data/auto-qualify/QualifyFooWithNoAlias.hs"]
      let [prog] = programs
      let importNode = Parse.findNode (AST.cast @H.ImportP) (AST.getDynNode prog.node)
      let fooUsageNode = Parse.findNode (\n -> if AST.nodeToText n == "foo" then Just n else Nothing) (AST.getDynNode prog.node)
      
      case (importNode, fooUsageNode) of
        (Just import', Just usageNode) -> do
          let hirImport = fromRight (error "Failed to parse import") $ Parse.parseImport import'
              edit = qualifyDeclUsage usageNode hirImport
              changes = getChanges edit
          case changes of
            [Change insertedText _] -> insertedText `shouldBe` "TestTypes.foo"
            _ -> expectationFailure $ show changes
            
        _ -> expectationFailure "Import or usage not found"