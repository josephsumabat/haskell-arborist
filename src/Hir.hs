module Hir where

import AST qualified
import Hir.Types

declName :: Decl -> Name
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

declDynNode :: Decl -> AST.DynNode
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
