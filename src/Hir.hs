module Hir where

import AST qualified
import Control.Error
import Data.Text (Text)
import Hir.Types

declNameText :: Decl hirKind -> Text
declNameText decl = (declName decl).nameText

declName :: Decl hirKind -> Name hirKind
declName decl =
  case decl of
    DeclData v -> v.name
    DeclNewtype v -> v.name
    DeclClass v -> v.name
    DeclSig v -> v.name
    DeclBind v -> v.name
    DeclDataFamily v -> v.name
    DeclPatternSig v -> v.name
    DeclPattern v -> v.name
    DeclTypeFamily v -> v.name
    DeclTypeSynonym v -> v.name

declDynNode :: Decl hirKind -> AST.DynNode
declDynNode decl =
  case decl of
    DeclData v -> AST.getDynNode v.node
    DeclNewtype v -> AST.getDynNode v.node
    DeclClass v -> AST.getDynNode v.node
    DeclSig v -> AST.getDynNode v.node
    DeclBind v -> AST.getDynNode v.node
    DeclDataFamily v -> AST.getDynNode v.node
    DeclPatternSig v -> AST.getDynNode v.node
    DeclPattern v -> AST.getDynNode v.node
    DeclTypeFamily v -> AST.getDynNode v.node
    DeclTypeSynonym v -> AST.getDynNode v.node

exportItemMods :: [ExportItem hirKind] -> [ModuleName]
exportItemMods exports =
  mapMaybe exportItemToMod exports
 where
  exportItemToMod :: ExportItem hirKind -> Maybe ModuleName
  exportItemToMod (ExportModuleItem mod) = Just mod
  exportItemToMod _ = Nothing

exportItemNames :: [ExportItem hirKind] -> [Qualified hirKind]
exportItemNames exports =
  mapMaybe exportItemToName exports
 where
  exportItemToName :: ExportItem hirKind -> Maybe (Qualified hirKind)
  exportItemToName (ExportItem {name}) = Just name
  exportItemToName _ = Nothing
