module Arborist.Scope.Types where

import AST
import AST.Haskell qualified as AST
import Arborist.Haddock
import Control.Applicative
import Data.HashMap.Lazy qualified as Map
import Data.Hashable
import Data.LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Set.NonEmpty qualified as NES
import Data.Text qualified as T
import Hir.Read.Types qualified as Hir
import Hir.Types

-- | An intermediate representation of a declaration annotated
data GlblDeclInfo = GlblDeclInfo
  { name :: T.Text
  , decl :: Hir.Decl
  , originatingMod :: ModuleText
  , importedFrom :: ImportInfo
  , requiresQualifier :: Bool
  }
  deriving (Show, Eq)

-- | A module namespace can be an alias or a real module
type ModNamespace = ModuleText

data ImportInfo = ImportInfo
  { mod :: ModuleText
  , namespace :: ModNamespace
  }
  deriving (Show, Eq, Ord)

instance Hashable ImportInfo where
  hashWithSalt s n = hashWithSalt s (n.mod.text, n.namespace.text)

data VarType
  = VarSig Hir.SigDecl
  | VarTy

data GlblVarInfo = GlblVarInfo
  { sig :: Maybe Hir.SigDecl
  , binds :: [Hir.BindDecl]
  , importedFrom :: NES.NESet ImportInfo
  , originatingMod :: ModuleText
  , loc :: LineColRange
  , name :: Hir.Name
  , requiresQualifier :: Bool
  }
  deriving (Show, Eq)

data NameKind
  = DataDecl
  | NewtypeDecl
  | ClassDecl
  | TypeDecl
  | DataFamilyDecl
  | TypeFamilyDecl
  | TypeSynonymDecl
  deriving (Show, Eq)

data GlblNameInfo = GlblNameInfo
  { name :: Hir.Name
  , importedFrom :: NES.NESet ImportInfo
  , originatingMod :: ModuleText
  , loc :: LineColRange
  , requiresQualifier :: Bool
  , decl :: Hir.Decl
  , nameKind :: NameKind
  }
  deriving (Show, Eq)

data GlblConstructorInfo = GlblConstructorInfo
  { name :: Hir.Name
  , importedFrom :: NES.NESet ImportInfo
  , originatingMod :: ModuleText
  , loc :: LineColRange
  , requiresQualifier :: Bool
  , parentType :: GlblDeclInfo
  , node :: DynNode
  }
  deriving (Show, Eq)

glblVarInfoToQualified :: GlblVarInfo -> QualifiedName
glblVarInfoToQualified glbl =
  QualifiedName glbl.originatingMod glbl.name.nameText

glblNameInfoToQualified :: GlblNameInfo -> QualifiedName
glblNameInfoToQualified glblName = QualifiedName glblName.originatingMod glblName.name.nameText

-- | Collect global var infos that have the same qualified name
-- e.g. all global var infos with qualified name MyModule.fn1 will be collected
tryMergeGlblVarInfo :: [GlblVarInfo] -> [GlblVarInfo]
tryMergeGlblVarInfo =
  Map.elems . List.foldl' insert Map.empty
 where
  insert acc g =
    Map.insertWith mergeOne (g.originatingMod, g.name) g acc

  mergeOne g1 g2 =
    GlblVarInfo
      { sig = g1.sig <|> g2.sig
      , binds = g1.binds ++ g2.binds
      , importedFrom = NES.union g1.importedFrom g2.importedFrom
      , originatingMod = g1.originatingMod
      , name = g1.name
      , loc = min g1.loc g2.loc
      , requiresQualifier =
          if g1.requiresQualifier == g2.requiresQualifier
            then g1.requiresQualifier
            else False
      }

data LocalVarInfo
  = -- | FnArg
    LocalVarParam (NE.NonEmpty LocalParam)
  | LocalVarLet (NE.NonEmpty LocalDecl)
  | -- | where clause
    LocalVarWhere (NE.NonEmpty LocalDecl)
  | LocalVarBind (NE.NonEmpty LocalBind)
  deriving (Show)

lclVarInfoToLoc :: LocalVarInfo -> NE.NonEmpty LineColRange
lclVarInfoToLoc lclVarInfo =
  case lclVarInfo of
    LocalVarParam lclParams -> (.var.dynNode.nodeLineColRange) <$> lclParams
    LocalVarLet lclDecls -> (.loc) <$> lclDecls
    LocalVarWhere lclDecls -> (.loc) <$> lclDecls
    LocalVarBind lclBinds -> (.dynNode.nodeLineColRange) <$> lclBinds

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
  , loc :: LineColRange
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

resolvedLclVarToLoc :: ResolvedLocalVarInfo -> LineColRange
resolvedLclVarToLoc resolvedVar =
  case resolvedVar of
    VarInfoParam lclParam -> lclParam.var.dynNode.nodeLineColRange
    VarInfoLet lclDecl -> lclDecl.loc
    VarInfoWhere lclDecl -> lclDecl.loc
    VarInfoBind lclBind -> lclBind.dynNode.nodeLineColRange

-- | Var infos for a name indexed by module
-- TODO: change val to [GlblVarInfo]?
type GlblVarInfoMap = Map.HashMap T.Text ImportVarInfoMap

type ImportVarInfoMap = Map.HashMap ImportInfo [GlblVarInfo]
type ImportNameInfoMap = Map.HashMap ImportInfo [GlblNameInfo]
type GlblNameInfoMap = Map.HashMap T.Text ImportNameInfoMap
type GlblConstructorInfoMap = Map.HashMap T.Text ImportConstructorInfoMap
type ImportConstructorInfoMap = Map.HashMap ImportInfo [GlblConstructorInfo]

data Scope = Scope
  { glblVarInfo :: GlblVarInfoMap
  , lclVarInfo :: Map.HashMap T.Text LocalVarInfo
  , glblNameInfo :: GlblNameInfoMap
  , glblConstructorInfo :: GlblConstructorInfoMap
  }
  deriving (Show)

-- | Nodes which change the scope
type ScopeChanger =
  AST.HaskellP
    AST.:+ AST.ImportP
    AST.:+ AST.FunctionP
    AST.:+ AST.LetInP
    AST.:+ AST.LetP
    AST.:+ AST.BindP
    AST.:+ AST.AlternativeP
    AST.:+ AST.LambdaP
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
    , glblNameInfo = Map.empty
    , glblConstructorInfo = Map.empty
    }
