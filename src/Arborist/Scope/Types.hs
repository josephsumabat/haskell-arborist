module Arborist.Scope.Types where

import AST
import Data.HashMap.Lazy qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Hir.Types (Decl, ModuleText)
import Hir.Types qualified as Hir

data GlblNameInfo = GlblNameInfo
  { name :: T.Text
  , decl :: Decl
  , dynNode :: DynNode
  , originatingMod :: ModuleText
  , importedFrom :: ModuleText
  , requiresQualifier :: Bool
  }
  deriving (Show)

data VarType
  = VarSig Hir.SigDecl
  | VarTy

data GlblVarInfo = GlblVarInfo
  { sig :: Maybe Hir.SigDecl
  , binds :: [Hir.BindDecl]
  , importedFrom :: ModuleText
  , originatingMod :: ModuleText
  }
  deriving (Show)

data LocalVarInfo
  = -- | FnArg
    LocalVarParam (NE.NonEmpty LocalParam)
  | LocalVarLet (NE.NonEmpty LocalDecl)
  | -- | where clause
    LocalVarWhere (NE.NonEmpty LocalDecl)
  | LocalVarBind (NE.NonEmpty LocalBind)
  deriving (Show)

data LocalBind = LocalBind
  { dynNode :: Node
  }
  deriving (Show)

data LocalParam = LocalParam
  { var :: Hir.Variable
  }
  deriving (Show)


data LocalDecl = LocalDecl
  { sig :: Maybe Hir.SigDecl
  , binds :: [Hir.BindDecl]
  }
  deriving (Show)

data VarInfo
  = VarInfoGlobal GlblVarInfo
  | VarInfoParam LocalParam
  | VarInfoLet LocalDecl
  | VarInfoWhere LocalDecl
  | VarInfoBind LocalBind
  deriving (Show)

-- | Var infos for a name indexed by module
type GlblVarInfoMap = Map.HashMap T.Text ModVarInfoMap

type ModVarInfoMap = Map.HashMap ModuleText [GlblVarInfo]

data Scope = Scope
  { glblVarInfo :: GlblVarInfoMap
  , lclVarInfo :: Map.HashMap T.Text LocalVarInfo
  }
  deriving (Show)

type ProgramIndex = Map.HashMap ModuleText Hir.Program
