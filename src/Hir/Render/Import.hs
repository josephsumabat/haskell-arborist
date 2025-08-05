module Hir.Render.Import (
  renderImport,
  fromReadImport,
  fromReadImportChildren,
  fromReadImportItem,
  fromReadImportList,
  addToImportList,
) where

import AST qualified
import Data.Text qualified as Text
import Hir.Types qualified as Hir

-- | Convert Hir.Import to Import
fromReadImport :: Hir.Import Hir.HirRead -> Hir.Import Hir.HirWrite
fromReadImport imp =
  Hir.Import
    { mod = imp.mod
    , alias = imp.alias
    , qualified = imp.qualified
    , hiding = imp.hiding
    , importList = fromReadImportList imp.importList
    , dynNode = ()
    }

fromReadImportList :: Maybe [Hir.ImportItem Hir.HirRead] -> Maybe [Hir.ImportItem Hir.HirWrite]
fromReadImportList importList = fmap (map fromReadImportItem) importList

-- | Convert Hir.ImportItem to ImportItem
fromReadImportItem :: Hir.ImportItem Hir.HirRead -> Hir.ImportItem Hir.HirWrite
fromReadImportItem item =
  Hir.ImportItem
    { namespace = item.namespace
    , name = fromReadName item.name
    , children = map fromReadImportChildren item.children
    }

-- | Convert Hir.ImportChildren to ImportChildren
fromReadImportChildren :: Hir.ImportChildren Hir.HirRead -> Hir.ImportChildren Hir.HirWrite
fromReadImportChildren child = case child of
  Hir.ImportAllChildren -> Hir.ImportAllChildren
  Hir.ImportChild namespace name -> Hir.ImportChild namespace (fromReadName name)

-- | Convert Hir.Name to Name
fromReadName :: Hir.Name Hir.HirRead -> Hir.Name Hir.HirWrite
fromReadName name =
  Hir.Name
    { nameText = name.nameText
    , isOperator = name.isOperator
    , isConstructor = name.isConstructor
    , dynNode = ()
    }

renderImport :: Hir.Import Hir.HirWrite -> Text.Text
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
renderImportItems :: [Hir.ImportItem Hir.HirWrite] -> Text.Text
renderImportItems = Text.intercalate ", " . map renderImportItem

-- | Render a single import item
renderImportItem :: Hir.ImportItem Hir.HirWrite -> Text.Text
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
renderImportChildren :: [Hir.ImportChildren Hir.HirWrite] -> Text.Text
renderImportChildren = Text.intercalate ", " . map renderImportChild

-- | Render a single import child
renderImportChild :: Hir.ImportChildren Hir.HirWrite -> Text.Text
renderImportChild child = case child of
  Hir.ImportAllChildren -> ".."
  Hir.ImportChild _namespace name ->
    let nameText = name.nameText
        wrappedName =
          if name.isOperator
            then "(" <> nameText <> ")"
            else nameText
     in wrappedName

addToImportList :: Hir.Import Hir.HirWrite -> Hir.ImportItem Hir.HirWrite -> Hir.Import Hir.HirWrite
addToImportList origImport importItem =
  ( origImport
      { Hir.importList =
          fmap (<> [importItem]) origImport.importList
      }
  )
