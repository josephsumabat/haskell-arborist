module Arborist.HirSpec (spec) where

import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir
import Test.Hspec
import Data.List.NonEmpty (NonEmpty(..))

spec :: Spec
spec = describe "renderImport" $ do
  describe "basic imports" $ do
    it "renders simple import" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text"

    it "renders qualified import" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = True
            , hiding = False
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified"

  describe "aliased imports" $ do
    it "renders import with alias" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = False
            , hiding = False
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text as T"

    it "renders qualified import with alias" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = False
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T"

  describe "hiding imports" $ do
    it "renders hiding import" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = True
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text hiding"

    it "renders qualified hiding import" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = True
            , hiding = True
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified hiding"

  describe "import lists" $ do
    it "renders import with single item" $ do
      let name = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text)"

    it "renders import with multiple items" $ do
      let name1 = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          name2 = Hir.Name
            { nameText = "String"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem1 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name1
            , children = []
            }
          importItem2 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name2
            , children = []
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem1, importItem2]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text, String)"

    it "renders import with complex multiple items" $ do
      let name1 = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          name2 = Hir.Name
            { nameText = "++"
            , isOperator = True
            , isConstructor = False
            , dynNode = ()
            }
          name3 = Hir.Name
            { nameText = "String"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          childName = Hir.Name
            { nameText = "Child"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem1 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name1
            , children = []
            }
          importItem2 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name2
            , children = []
            }
          importItem3 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name3
            , children = [Hir.ImportChild Hir.NameSpaceValue childName]
            }
          importItem4 = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = Hir.Name { nameText = "Data", isOperator = False, isConstructor = False, dynNode = ()}
            , children = [Hir.ImportAllChildren]
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem1, importItem2, importItem3, importItem4]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text (\n    Text,\n    (++),\n    String (Child),\n    Data (..)\n  )"

  describe "complex imports" $ do
    it "renders qualified import with alias and hiding" $ do
      let imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = True
            , importList = Nothing
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T hiding"

    it "renders qualified import with alias and import list" $ do
      let name = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Just (Hir.ModuleText { parts = "T" :| [], text = "T" })
            , qualified = True
            , hiding = False
            , importList = Just [importItem]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text qualified as T (Text)"

  describe "import items with children" $ do
    it "renders import item with (..)" $ do
      let name = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = [Hir.ImportAllChildren]
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text (..))"

    it "renders import item with specific children" $ do
      let name = Hir.Name
            { nameText = "Text"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          childName = Hir.Name
            { nameText = "Child"
            , isOperator = False
            , isConstructor = False
            , dynNode = ()
            }
          importItem = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = [Hir.ImportChild Hir.NameSpaceValue childName]
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text (Text (Child))"

  describe "operator imports" $ do
    it "renders operator import" $ do
      let name = Hir.Name
            { nameText = "++"
            , isOperator = True
            , isConstructor = False
            , dynNode = ()
            }
          importItem = Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = name
            , children = []
            }
          imp = Hir.Import
            { mod = Hir.ModuleText { parts = "Data" :| ["Text"], text = "Data.Text" }
            , alias = Nothing
            , qualified = False
            , hiding = False
            , importList = Just [importItem]
            , dynNode = ()
            }
      Render.renderImport imp `shouldBe` "import Data.Text ((++))"
