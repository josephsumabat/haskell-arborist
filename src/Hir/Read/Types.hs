{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type aliases for Hir types with HirRead parameter
module Hir.Read.Types where

import Hir.Types

-- | Type aliases for all Hir types with HirRead parameter

-- Basic types
type Name = Hir.Types.Name HirRead

type Qualified = Hir.Types.Qualified HirRead
type Import = Hir.Types.Import HirRead
type ImportItem = Hir.Types.ImportItem HirRead
type ImportChildren = Hir.Types.ImportChildren HirRead
type ExportItem = Hir.Types.ExportItem HirRead
type ExportChildren = Hir.Types.ExportChildren HirRead

-- Declaration types
type DataDecl = Hir.Types.DataDecl HirRead
type ClassDecl = Hir.Types.ClassDecl HirRead
type BindDecl = Hir.Types.BindDecl HirRead
type SigDecl = Hir.Types.SigDecl HirRead
type DataFamilyDecl = Hir.Types.DataFamilyDecl HirRead
type NewtypeDecl = Hir.Types.NewtypeDecl HirRead
type PatternSigDecl = Hir.Types.PatternSigDecl HirRead
type PatternDecl = Hir.Types.PatternDecl HirRead
type TypeFamilyDecl = Hir.Types.TypeFamilyDecl HirRead
type TypeSynonymDecl = Hir.Types.TypeSynonymDecl HirRead
type Decl = Hir.Types.Decl HirRead
type Program = Hir.Types.Program HirRead

-- Pattern and variable types
type NameOrPat = Hir.Types.NameOrPat HirRead
type Pattern = Hir.Types.Pattern HirRead
type Variable = Hir.Types.Variable HirRead
type Param = Hir.Types.Param HirRead
type Params = Hir.Types.Params HirRead
type FunctionBind = Hir.Types.FunctionBind HirRead
type LocalDecls = Hir.Types.LocalDecls HirRead
