module Hir where

import Hir.Types
import qualified AST
import Data.Foldable

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

patternNames :: Pattern -> [Name]
patternNames pat = go pat []
  where
    go :: Pattern -> [Name] -> [Name]
    go PatWildcard acc = acc
    go (PatVariable name) acc = name : acc
    go (PatConstructor name pats) acc = foldl' (flip go) (name : acc) pats
    go (PatTuple pats) acc = foldl' (flip go) acc pats
    go (PatList pats) acc = foldl' (flip go) acc pats
    go (PatLiteral _) acc = acc
    go (PatAs name pat) acc = go pat (name : acc)
    go (PatInfix p1 name p2) acc = go p1 (go p2 (name : acc))
    go (PatStrict pat) acc = go pat acc
    go (PatLazy pat) acc = go pat acc
    go (PatView _ pat) acc = go pat acc
    go (PatQuasiquote name pat) acc = go pat (name : acc)
