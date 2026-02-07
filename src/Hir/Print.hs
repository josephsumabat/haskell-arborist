module Hir.Print (
  printExportItem,
  printExportItems,
)
where

import Data.Text.Lazy qualified as TL
import Hir.Types

printName :: Name hirKind -> TL.Text
printName name = TL.fromStrict name.nameText

printModuleName :: ModuleName -> TL.Text
printModuleName name = TL.fromStrict name.mod.text

printQualified :: Qualified hirKind -> TL.Text
printQualified (Qualified {mod, name}) =
  case mod of
    Nothing -> printName name
    Just modName -> TL.concat [printModuleName modName, ".", printName name]

printExportChildren :: ExportChildren hirKind -> TL.Text
printExportChildren export =
  case export of
    ExportAllChildren -> ".."
    ExportChild _namespace name -> printQualified name

printExportItem :: ExportItem hirKind -> TL.Text
printExportItem export =
  case export of
    ExportItem {namespace = _namespace, name, children} ->
      printQualified name <> TL.intercalate ", " (map printExportChildren children)
    ExportModuleItem mod -> "module " <> printModuleName mod

printExportItems :: [ExportItem hirKind] -> TL.Text
printExportItems exports = TL.intercalate ",\n" (map printExportItem exports)
