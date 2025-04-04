module Arborist.Scope.Types where

import AST
import AST.Haskell qualified as AST
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
  , name :: Hir.Name
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
  deriving (Show, Eq)

data LocalParam = LocalParam
  { var :: Hir.Variable
  }
  deriving (Show, Eq)

data LocalDecl = LocalDecl
  { sig :: Maybe Hir.SigDecl
  , binds :: [Hir.BindDecl]
  }
  deriving (Show, Eq)

data ResolvedVarInfo
  = ResolvedGlobal GlblVarInfo
  | ResolvedLocal ResolvedLocalVarInfo
  deriving (Show)

instance Eq ResolvedVarInfo where
  (==) (ResolvedGlobal a) (ResolvedGlobal b) = a.originatingMod == b.originatingMod && a.name == b.name
  (==) (ResolvedLocal a) (ResolvedLocal b) = a == b
  (==) _ _ = False

data ResolvedLocalVarInfo
  = VarInfoParam LocalParam
  | VarInfoLet LocalDecl
  | VarInfoWhere LocalDecl
  | VarInfoBind LocalBind
  deriving (Show, Eq)

-- | Var infos for a name indexed by module
type GlblVarInfoMap = Map.HashMap T.Text ModVarInfoMap

type ModVarInfoMap = Map.HashMap ModuleText [GlblVarInfo]

data Scope = Scope
  { glblVarInfo :: GlblVarInfoMap
  , lclVarInfo :: Map.HashMap T.Text LocalVarInfo
  }
  deriving (Show)

-- | Nodes which change the scope
type ScopeChanger =
  AST.HaskellP
    AST.:+ AST.FunctionP
    AST.:+ AST.LetInP
    AST.:+ AST.LetP
    AST.:+ AST.BindP
    AST.:+ AST.AlternativeP
    AST.:+ AST.Nil

type DoBind =
  AST.BindP
    AST.:+ AST.LetP
    AST.:+ AST.Nil

emptyScope :: Scope
emptyScope =
  Scope
    { glblVarInfo = Map.empty
    , lclVarInfo = Map.empty
    }

