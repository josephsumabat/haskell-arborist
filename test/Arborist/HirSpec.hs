module Arborist.HirSpec (spec) where

import Hir.Render.Import qualified as Render
import Arborist.Rewrite (rewriteNode)
import Data.Text qualified as Text
import Hir.Types qualified as Hir
import Test.Hspec
import AST qualified
import AST.Haskell qualified as H
import Data.List.NonEmpty (NonEmpty(..))

spec :: Spec
spec = describe "renderImport" $ do
  describe "basic imports" $ do
    it "renders simple import" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text"

    it "renders qualified import" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = True
            , hiding = False
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified"

  describe "aliased imports" $ do
    it "renders import with alias" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = False
            , hiding = False
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text as T"

    it "renders qualified import with alias" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = False
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T"

  describe "hiding imports" $ do
    it "renders hiding import" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = True
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text hiding"

    it "renders qualified hiding import" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = True
            , hiding = True
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified hiding"

  describe "import lists" $ do
    it "renders import with single item" $ do
      let name = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          importItem = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text)"

    it "renders import with multiple items" $ do
      let name1 = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          name2 = Render.Name
            { nameText = "String"
            , isOperator = False
            , isConstructor = False
            }
          importItem1 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name1
            , children = []
            }
          importItem2 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name2
            , children = []
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem1, importItem2]
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text, String)"

    it "renders import with complex multiple items" $ do
      let name1 = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          name2 = Render.Name
            { nameText = "++"
            , isOperator = True
            , isConstructor = False
            }
          name3 = Render.Name
            { nameText = "String"
            , isOperator = False
            , isConstructor = False
            }
          childName = Render.Name
            { nameText = "Child"
            , isOperator = False
            , isConstructor = False
            }
          importItem1 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name1
            , children = []
            }
          importItem2 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name2
            , children = []
            }
          importItem3 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name3
            , children = [Render.ImportChild Hir.NameSpaceValue childName]
            }
          importItem4 = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = Render.Name { nameText = "Data", isOperator = False, isConstructor = False }
            , children = [Render.ImportAllChildren]
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem1, importItem2, importItem3, importItem4]
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text, (++), String (Child), Data (..))"

  describe "complex imports" $ do
    it "renders qualified import with alias and hiding" $ do
      let imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = True
            , importList = Nothing
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T hiding"

    it "renders qualified import with alias and import list" $ do
      let name = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          importItem = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = False
            , importList = Just [importItem]
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T (Text)"

  describe "import items with children" $ do
    it "renders import item with (..)" $ do
      let name = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          importItem = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = [Render.ImportAllChildren]
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text (..))"

    it "renders import item with specific children" $ do
      let name = Render.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            }
          childName = Render.Name
            { nameText = "Child"
            , isOperator = False
            , isConstructor = False
            }
          importItem = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = [Render.ImportChild Hir.NameSpaceValue childName]
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text (Child))"

  describe "operator imports" $ do
    it "renders operator import" $ do
      let name = Render.Name
            { nameText = "++"
            , isOperator = True
            , isConstructor = False
            }
          importItem = Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Render.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            }
      Render.renderImport imp `shouldBe` "import Data.Text ((++))"

  describe "real parsed imports" $ do
    it "renders real import from test data" $ do
      -- This test uses real parsed data to test the full functionality
      pendingWith "Need to implement with real parsed test data" 
