{-# LANGUAGE OverloadedStrings #-}

module Arborist.BuildGraphSpec (spec) where

import BuildGraph.Directory (
  BuildGraphOutput (..),
  DirName (..),
  ModuleInfo (..),
  ModuleLocation (..),
  RootDirectory (..),
  TargetOutput (..),
  moduleTargetsFromInfos,
 )
import BuildGraph.ModuleTargetOverrides (ModuleTargetOverrides (..))
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Hir.Parse (parseModuleTextFromText)
import Hir.Types qualified as Hir
import Test.Hspec

spec :: Spec
spec = describe "moduleTargetsFromInfos" $ do
  let rootInfo =
        RootDirectory
          { rootPath = "/repo"
          , rootLabel = "."
          , sourcePrefix = Nothing
          }
      rootDir = DirName "."
      buckDirs = Set.fromList [DirName "has_buck"]
      overrides = ModuleTargetOverrides HM.empty
      moduleMap =
        HM.fromList
          [ moduleEntry "HasBuck.Child" "has_buck/child"
          , moduleEntry "NeedsRoot.Child" "needs_root/child"
          , persistentModuleEntry
          ]
      moduleEntry :: T.Text -> T.Text -> (Hir.ModuleText, ModuleInfo)
      moduleEntry name dirText =
        let moduleName = parseModuleTextFromText name
         in
            ( moduleName
            , ModuleInfo
                { name = moduleName
                , location = LocalModule (DirName dirText)
                , imports = Set.empty
                , sourceFile = Just (dirText <> "/Module.hs")
                , persistentModels = []
                }
            )
      persistentModuleEntry =
        let moduleName = parseModuleTextFromText "Example.PersistentModels.Invoice"
            baseInfo = snd (moduleEntry "Example.PersistentModels.Invoice" "persistent_models/invoice")
         in
            ( moduleName
            , baseInfo {persistentModels = ["ar_invoice"]}
            )
      lookupDirectory BuildGraphOutput {targets} moduleName =
        targetDirectory
          <$> List.find (elem moduleName . targetModules) targets

  context "when an override file is provided" $ do
    let graph = moduleTargetsFromInfos rootInfo rootDir buckDirs True moduleMap overrides
    it "keeps modules under directories that already have BUCK files" $ do
      lookupDirectory graph "HasBuck.Child" `shouldBe` Just "has_buck"
    it "moves all other modules to the root directory" $ do
      lookupDirectory graph "NeedsRoot.Child" `shouldBe` Just "."

  context "without an override file" $ do
    let graph = moduleTargetsFromInfos rootInfo rootDir buckDirs False moduleMap overrides
    it "leaves module directories unchanged" $ do
      lookupDirectory graph "NeedsRoot.Child" `shouldBe` Just "needs_root/child"
    it "renders module target names with directory prefixes" $ do
      lookupTargetName graph "NeedsRoot.Child" `shouldBe` Just "//needs_root/child:needsroot_child"
    it "includes short and full target names on target outputs" $ do
      lookupTargetFields graph "NeedsRoot.Child" `shouldBe` Just ("needsroot_child", "//needs_root/child:needsroot_child")
    it "captures persistent model dependencies in srcDeps" $ do
      lookupSrcDeps graph "Example.PersistentModels.Invoice"
        `shouldBe` Just [("persistent_models/invoice/Module.hs", ["config/modelsFiles/ar_invoice.persistentmodels"])]

  where
    targetModules TargetOutput {modules = mods} = mods
    targetDirectory TargetOutput {directory = dirText} = dirText
    lookupTargetName BuildGraphOutput {moduleToTarget} moduleName =
      HM.lookup moduleName moduleToTarget
    lookupTargetFields BuildGraphOutput {targets} moduleName =
      listToMaybe
        [ (target.targetName, target.targetKey)
        | target <- targets
        , moduleName `elem` targetModules target
        ]
    lookupSrcDeps BuildGraphOutput {targets} moduleName =
      listToMaybe
        [ target.srcDeps
        | target <- targets
        , moduleName `elem` targetModules target
        ]
