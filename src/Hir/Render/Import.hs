module Hir.Render.Import (renderImport, Import (..), ImportItem (..), ImportChildren (..), Name (..), fromHirImport) where

import AST qualified
import Data.Text qualified as Text
import Hir.Types qualified as Hir

-- | Import without DynNode fields
data Import = Import
  { mod :: Hir.ModuleText
  , alias :: Maybe Hir.ModuleText
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: Maybe [ImportItem]
  }
  deriving (Show, Eq)

-- | ImportItem without DynNode fields
data ImportItem = ImportItem
  { namespace :: Hir.NameSpace
  , name :: Name
  , children :: [ImportChildren]
  }
  deriving (Show, Eq)

-- | ImportChildren without DynNode fields
data ImportChildren
  = ImportAllChildren
  | ImportChild Hir.NameSpace Name
  deriving (Show, Eq)

-- | Name without DynNode field
data Name = Name
  { nameText :: !Text.Text
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }
  deriving (Show, Eq)

-- | Convert Hir.Import to Import
fromHirImport :: Hir.Import -> Import
fromHirImport imp =
  Import
    { mod = imp.mod
    , alias = imp.alias
    , qualified = imp.qualified
    , hiding = imp.hiding
    , importList = fmap (map fromHirImportItem) imp.importList
    }

-- | Convert Hir.ImportItem to ImportItem
fromHirImportItem :: Hir.ImportItem -> ImportItem
fromHirImportItem item =
  ImportItem
    { namespace = item.namespace
    , name = fromHirName item.name
    , children = map fromHirImportChildren item.children
    }

-- | Convert Hir.ImportChildren to ImportChildren
fromHirImportChildren :: Hir.ImportChildren -> ImportChildren
fromHirImportChildren child = case child of
  Hir.ImportAllChildren -> ImportAllChildren
  Hir.ImportChild namespace name -> ImportChild namespace (fromHirName name)

-- | Convert Hir.Name to Name
fromHirName :: Hir.Name -> Name
fromHirName name =
  Name
    { nameText = Hir.nameText name
    , isOperator = name.isOperator
    , isConstructor = name.isConstructor
    }

renderImport :: Import -> Text.Text
renderImport imp =
  let base = "import " <> imp.mod.text
      withQualified = if imp.qualified then base <> " qualified" else base
      withAlias = case imp.alias of
        Just alias -> withQualified <> " as " <> alias.text
        Nothing -> withQualified
      withHiding = if imp.hiding then withAlias <> " hiding" else withAlias
      withImportList = case imp.importList of
        Just items -> withHiding <> " (" <> renderImportItems items <> ")"
        Nothing -> withHiding
   in withImportList

-- | Render a list of import items
renderImportItems :: [ImportItem] -> Text.Text
renderImportItems = Text.intercalate ", " . map renderImportItem

-- | Render a single import item
renderImportItem :: ImportItem -> Text.Text
renderImportItem item =
  let nameText = item.name.nameText
      wrappedName =
        if item.name.isOperator
          then "(" <> nameText <> ")"
          else nameText
      withChildren = case item.children of
        [] -> wrappedName
        children -> wrappedName <> " (" <> renderImportChildren children <> ")"
   in withChildren

-- | Render import children
renderImportChildren :: [ImportChildren] -> Text.Text
renderImportChildren = Text.intercalate ", " . map renderImportChild

-- | Render a single import child
renderImportChild :: ImportChildren -> Text.Text
renderImportChild child = case child of
  ImportAllChildren -> ".."
  ImportChild namespace name ->
    let nameText = name.nameText
        wrappedName =
          if name.isOperator
            then "(" <> nameText <> ")"
            else nameText
     in wrappedName
