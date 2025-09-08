{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arborist.TransitiveReexportSpec (spec) where

import Arborist.Files
import Arborist.ProgramIndex
import Arborist.Scope.Global
import Arborist.Scope.Types
import Data.HashMap.Strict qualified as Map
import Hir.Parse qualified as Hir
import Hir.Types qualified as HirT
import System.FilePath
import Test.Hspec
import TestImport
import HaskellAnalyzer (parsePrg)

spec :: Spec
spec = do
  describe "transitive re-exports with hiding" $ do
    it "resolves myFunc to B.myFunc when A imports from B and B hides C.myFunc" $ do
      let baseDir = "./test-data/transitive-reexport"
      modFileMap <- buildModuleFileMap [baseDir]
      let Just fileA = Map.lookup (Hir.parseModuleTextFromText "A") modFileMap
      [prgA] <- getPrg [fileA]
      deps <- gatherScopeDeps Map.empty prgA modFileMap Nothing
      let exportIdx = Map.empty
      let avail = getGlobalAvailableDecls deps exportIdx prgA
      let scope = globalDeclsToScope avail (HirT.getImports prgA)
      let varMap = scope.glblVarInfo
      case Map.lookup "myFunc" varMap of
        Nothing -> expectationFailure "myFunc not in scope"
        Just importMap -> do
          let importInfos = Map.keys importMap
          -- Only one source, and it should be module B
          length importInfos `shouldBe` 1
          let [impInfo] = importInfos
          impInfo.mod `shouldBe` Hir.parseModuleTextFromText "B"
          -- And the originating module of the single GlblVarInfo should be B
          case Map.lookup impInfo importMap of
            Nothing -> expectationFailure "missing var info for import"
            Just [glblVar] -> glblVar.originatingMod `shouldBe` Hir.parseModuleTextFromText "B"
            Just xs -> expectationFailure ("expected single GlblVarInfo, got " <> show (length xs))

    it "resolves myFunc2 via explicit import list through B2 re-export" $ do
      let baseDir = "./test-data/transitive-reexport"
      modFileMap <- buildModuleFileMap [baseDir]
      let Just fileA2 = Map.lookup (Hir.parseModuleTextFromText "A2") modFileMap
      [prgA2] <- getPrg [fileA2]
      deps <- gatherScopeDeps Map.empty prgA2 modFileMap Nothing
      let exportIdx = Map.empty
      let avail = getGlobalAvailableDecls deps exportIdx prgA2
      let scope = globalDeclsToScope avail (HirT.getImports prgA2)
      case Map.lookup "myFunc2" scope.glblVarInfo of
        Nothing -> expectationFailure "myFunc2 not in scope"
        Just importMap -> do
          let importInfos = Map.keys importMap
          length importInfos `shouldBe` 1
          let [impInfo] = importInfos
          impInfo.mod `shouldBe` Hir.parseModuleTextFromText "B2"
          case Map.lookup impInfo importMap of
            Nothing -> expectationFailure "missing var info for import"
            Just [glblVar] -> glblVar.originatingMod `shouldBe` Hir.parseModuleTextFromText "C2"
            Just xs -> expectationFailure ("expected single GlblVarInfo, got " <> show (length xs))

    it "resolves overlapping symbols correctly when re-exported from different modules" $ do
      let baseDir = "./test-data/transitive-reexport"
      modFileMap <- buildModuleFileMap [baseDir]
      -- Create a synthetic top module A3 that imports both B3 and B
      let a3Text = "module A3 (u1,u2) where\nimport B3 (myFunc, myFunc2)\nimport B (myFunc)\nu1 = myFunc\nu2 = myFunc2\n"
      let (_, prgA3) = parsePrg a3Text
      deps <- gatherScopeDeps Map.empty prgA3 modFileMap Nothing
      let exportIdx = Map.empty
      let avail = getGlobalAvailableDecls deps exportIdx prgA3
      let scope = globalDeclsToScope avail (HirT.getImports prgA3)
      case Map.lookup "myFunc" scope.glblVarInfo of
        Nothing -> expectationFailure "myFunc not in scope"
        Just importMap -> do
          -- Prefer B over C when both available through B3 re-export, due to direct B import
          let importMods = map (.mod) (Map.keys importMap)
          importMods `shouldSatisfy` (elem (Hir.parseModuleTextFromText "B"))
      case Map.lookup "myFunc2" scope.glblVarInfo of
        Nothing -> expectationFailure "myFunc2 not in scope"
        Just importMap -> do
          let importMods = map (.mod) (Map.keys importMap)
          importMods `shouldSatisfy` (elem (Hir.parseModuleTextFromText "B3")) 
