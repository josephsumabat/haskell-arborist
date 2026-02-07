module Arborist.PersistentModelsSpec (spec) where

import Arborist.PersistentModels
import Test.Hspec
import TestImport (getPrg)

spec :: Spec
spec = describe "Persistent model splice detection" $ do
  it "collects a single model file from mkModel splices" $ do
    [prg] <- getPrg ["test-data/persistent-models/PersistentModels/Invoice.hs"]
    requiredModelFiles prg `shouldBe` ["ar_invoice"]

  it "collects multiple files and supports alternate helpers" $ do
    [prg] <- getPrg ["test-data/persistent-models/PersistentModels/Multiple.hs"]
    requiredModelFiles prg `shouldBe` ["foo_model", "bar_model"]

  it "requires a known PersistentModels import to consider decls" $ do
    [prg] <- getPrg ["test-data/persistent-models/PersistentModels/NoImport.hs"]
    requiredModelFiles prg `shouldBe` []

  it "tracks which modules enable detection" $ do
    persistentModelImportModules `shouldBe` ["PersistentModels.Import"]
