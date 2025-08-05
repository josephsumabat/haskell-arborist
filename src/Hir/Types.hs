{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Hir represents a simplified parse tree
module Hir.Types where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import AST.Sum (Nil, (:+))
import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.Kind qualified as Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | An hir node with a dyn node
data HirRead

-- | An hir node with a read node
data HirWrite

class (Show (XDynNode s), Eq (XDynNode s)) => HirDynNode s where
  type XDynNode s :: Kind.Type

instance HirDynNode HirRead where
  type XDynNode HirRead = DynNode

instance HirDynNode HirWrite where
  type XDynNode HirWrite = ()

data NameSpace
  = NameSpaceValue
  | NameSpaceType
  | NameSpacePattern
  deriving (Show, Eq, Generic)

instance Hashable NameSpace

type ThSplice = Haskell.TopSpliceP :+ Haskell.SpliceP

data Name s
  = Name
  { dynNode :: (XDynNode s)
  , nameText :: !Text
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }

deriving instance (HirDynNode s) => Show (Name s)

instance Eq (Name s) where
  (==) = (==) `on` (.nameText)

instance Hashable (Name s) where
  hashWithSalt salt name = hashWithSalt salt name.nameText

data Qualified s = Qualified
  { mod :: Maybe ModuleName
  , name :: (Name s)
  }
  deriving (Show, Eq, Generic)

instance Hashable (Qualified s)

data ModuleText = ModuleText
  { parts :: NonEmpty Text
  , text :: Text
  }
  deriving (Ord)

instance Show ModuleText where
  show m = T.unpack m.text

instance Eq ModuleText where
  (==) = (==) `on` (.text)

instance Hashable ModuleText where
  hashWithSalt salt ModuleText {text} = hashWithSalt salt text

data ModuleName = ModuleName
  { mod :: ModuleText
  , node :: H.ModuleP
  }
  deriving (Show)

instance Eq ModuleName where
  (==) = (==) `on` (.mod)

instance Ord ModuleName where
  compare a b = compare a.mod b.mod

instance Hashable ModuleName where
  hashWithSalt salt ModuleName {mod} = hashWithSalt salt mod

data ImportChildren s
  = ImportAllChildren
  | ImportChild NameSpace (Name s)
  deriving (Show, Eq)

data ImportItem s = ImportItem
  { namespace :: NameSpace
  , name :: Name s
  , children :: [ImportChildren s]
  }
  deriving (Show, Eq)

data ExportChildren s
  = ExportAllChildren
  | ExportChild NameSpace (Qualified s)
  deriving (Show, Eq, Generic)

instance Hashable (ExportChildren s)

data ExportItem s
  = ExportItem
      { namespace :: NameSpace
      , name :: (Qualified s)
      , children :: [ExportChildren s]
      }
  | ExportModuleItem ModuleName
  deriving (Show, Eq, Generic)

instance Hashable (ExportItem s)

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import s = Import
  { mod :: ModuleText
  , alias :: Maybe ModuleText
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: Maybe [(ImportItem s)]
  , dynNode :: (XDynNode s)
  }

deriving instance (HirDynNode s) => Show (Import s)
deriving instance (HirDynNode s) => Eq (Import s)

-- pattern OpenImport :: ModuleText -> AST.DynNode -> (Import Read)
-- pattern OpenImport mod dynNode = Import {mod, alias = Nothing, qualified = False, hiding = False, importList = Nothing, dynNode}
--
type ParseNameTypes =
  Haskell.NameP
    :+ Haskell.ConstructorP
    :+ Haskell.VariableP
    :+ Haskell.OperatorP
    :+ Haskell.FieldNameP
    :+ Haskell.ConstructorOperatorP
    :+ Nil

type ParseQualifiedTypes = H.QualifiedP :+ ParseNameTypes

data DataDecl s = DataDecl
  { name :: Name s
  , node :: H.DataTypeP
  }
  deriving (Show, Eq)

data ClassDecl s = ClassDecl
  { name :: Name s
  , node :: H.ClassP
  }
  deriving (Show, Eq)

data BindDecl s = BindDecl
  { name :: Name s
  , node :: H.BindP :+ H.FunctionP :+ AST.Nil
  }
  deriving (Show, Eq)

data NameOrPat s
  = IsName (Name s)
  | IsPat (Pattern s)
  deriving (Show, Eq)

data SigDecl s = SigDecl
  { name :: Name s
  , node :: H.SignatureP
  }
  deriving (Show, Eq)

data DataFamilyDecl s = DataFamilyDecl
  { name :: Name s
  , node :: H.DataFamilyP
  }
  deriving (Show, Eq)

data NewtypeDecl s = NewtypeDecl
  { name :: Name s
  , node :: H.NewtypeP
  }
  deriving (Show, Eq)

data PatternSigDecl s = PatternSigDecl
  { name :: Name s
  , node :: H.SignatureP
  }
  deriving (Show, Eq)

data PatternDecl s = PatternDecl
  { name :: Name s
  , node :: H.EquationP
  }
  deriving (Show, Eq)

data TypeFamilyDecl s = TypeFamilyDecl
  { name :: Name s
  , node :: H.TypeFamilyP
  }
  deriving (Show, Eq)

data TypeSynonymDecl s = TypeSynonymDecl
  { name :: Name s
  , node :: H.TypeSynomymP
  }
  deriving (Show, Eq)

data Decl s
  = DeclData (DataDecl s)
  | DeclNewtype (NewtypeDecl s)
  | DeclClass (ClassDecl s)
  | DeclSig (SigDecl s)
  | DeclBind (BindDecl s)
  | DeclDataFamily (DataFamilyDecl s)
  | DeclPatternSig (PatternSigDecl s)
  | DeclPattern (PatternDecl s)
  | DeclTypeFamily (TypeFamilyDecl s)
  | DeclTypeSynonym (TypeSynonymDecl s)
  deriving (Show, Eq)

data Program s = Program
  { mod :: Maybe ModuleText
  , imports :: [Import s]
  , exports :: Maybe [ExportItem s]
  , decls :: [Decl s]
  , node :: Haskell.HaskellP
  }
  deriving (Show, Eq)

getImports :: Program s -> [Import s]
getImports prg = prg.imports

type GetNameTypes =
  Haskell.NameP
    :+ Haskell.ConstructorP
    :+ Haskell.VariableP
    :+ Haskell.OperatorP
    :+ Haskell.FieldNameP
    :+ Haskell.ConstructorOperatorP
    :+ Nil

data ThQuotedName = ThQuotedName
  { isTy :: Bool
  , node :: AST.DynNode
  }

data FunctionBind s
  = FunctionBind
  { fnName :: Text
  , params :: [Pattern s]
  }
  deriving (Show, Eq)

data Params s = Params
  { params :: [Param s]
  }
  deriving (Show, Eq)

data Param s
  = ParamVar (Variable s)
  | ParamWildcard
  | ParamOther
  deriving (Show, Eq)

data Pattern s = Pattern
  { patVars :: [Variable s]
  }
  deriving (Show, Eq)

data Variable s = Variable
  { name :: Name s
  , dynNode :: DynNode
  }
  deriving (Show, Eq)

data LocalDecls s = LocalDecls
  { decls :: [Decl s]
  }
  deriving (Show, Eq)
