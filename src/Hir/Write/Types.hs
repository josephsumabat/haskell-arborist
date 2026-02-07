{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Type aliases for Hir types with HirWrite parameter
module Hir.Write.Types where

import Hir.Types

-- | Type aliases for all Hir types with HirWrite parameter

-- Basic types
type Name = Hir.Types.Name HirWrite

type Qualified = Hir.Types.Qualified HirWrite
type Import = Hir.Types.Import HirWrite
type ImportItem = Hir.Types.ImportItem HirWrite
type ImportChildren = Hir.Types.ImportChildren HirWrite
type ExportItem = Hir.Types.ExportItem HirWrite
type ExportChildren = Hir.Types.ExportChildren HirWrite

-- Declaration types
type DataDecl = Hir.Types.DataDecl HirWrite
type ClassDecl = Hir.Types.ClassDecl HirWrite
type BindDecl = Hir.Types.BindDecl HirWrite
type SigDecl = Hir.Types.SigDecl HirWrite
type DataFamilyDecl = Hir.Types.DataFamilyDecl HirWrite
type NewtypeDecl = Hir.Types.NewtypeDecl HirWrite
type PatternSigDecl = Hir.Types.PatternSigDecl HirWrite
type PatternDecl = Hir.Types.PatternDecl HirWrite
type TypeFamilyDecl = Hir.Types.TypeFamilyDecl HirWrite
type TypeSynonymDecl = Hir.Types.TypeSynonymDecl HirWrite
type Decl = Hir.Types.Decl HirWrite
type Program = Hir.Types.Program HirWrite

-- Pattern and variable types
type NameOrPat = Hir.Types.NameOrPat HirWrite
type Pattern = Hir.Types.Pattern HirWrite
type Variable = Hir.Types.Variable HirWrite
type Param = Hir.Types.Param HirWrite
type Params = Hir.Types.Params HirWrite
type FunctionBind = Hir.Types.FunctionBind HirWrite
type LocalDecls = Hir.Types.LocalDecls HirWrite
