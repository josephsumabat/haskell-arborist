-- | Hir represents a simplified parse tree
module Hir.Types where

import AST (DynNode)
import AST qualified
import AST.Haskell qualified as H
import AST.Haskell qualified as Haskell
import AST.Sum (Nil, (:+))
import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data NameSpace
  = NameSpaceValue
  | NameSpaceType
  | NameSpacePattern
  deriving (Show, Eq, Generic)

instance Hashable NameSpace

type ThSplice = Haskell.TopSpliceP :+ Haskell.SpliceP

data Name = Name
  { node :: !DynNode
  , isOperator :: !Bool
  , isConstructor :: !Bool
  }

data NameShow = NameShow {name :: Text, node :: DynNode}
  deriving (Show, Eq)

instance Show Name where
  show Name {node} = show NameShow {name = AST.nodeToText node, node}

instance Eq Name where
  (==) = (==) `on` (.node.nodeText)

instance Hashable Name where
  hashWithSalt salt name = hashWithSalt salt name.node.nodeText

data Qualified = Qualified
  { mod :: Maybe ModuleName
  , name :: Name
  }
  deriving (Show, Eq, Generic)

instance Hashable Qualified

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

data ImportChildren
  = ImportAllChildren
  | ImportChild NameSpace Name
  deriving (Show, Eq)

data ImportItem = ImportItem
  { namespace :: NameSpace
  , name :: Name
  , children :: [ImportChildren]
  }
  deriving (Show, Eq)

data ExportChildren
  = ExportAllChildren
  | ExportChild NameSpace Qualified
  deriving (Show, Eq, Generic)

instance Hashable ExportChildren

data ExportItem
  = ExportItem
      { namespace :: NameSpace
      , name :: Qualified
      , children :: [ExportChildren]
      }
  | ExportModuleItem ModuleName
  deriving (Show, Eq, Generic)

instance Hashable ExportItem

data ImportName = ImportName
  { name :: Text
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: ModuleText
  , alias :: Maybe ModuleText
  , qualified :: !Bool
  , hiding :: !Bool
  , importList :: [ImportItem]
  }
  deriving (Show, Eq)

pattern OpenImport :: ModuleText -> Import
pattern OpenImport mod = Import {mod, alias = Nothing, qualified = False, hiding = False, importList = []}

type ParseNameTypes =
  Haskell.NameP
    :+ Haskell.ConstructorP
    :+ Haskell.VariableP
    :+ Haskell.OperatorP
    :+ Haskell.FieldNameP
    :+ Haskell.ConstructorOperatorP
    :+ Nil

type ParseQualifiedTypes = H.QualifiedP :+ ParseNameTypes

data DataDecl = DataDecl
  { name :: Name
  , node :: H.DataTypeP
  }
  deriving (Show, Eq)

data ClassDecl = ClassDecl
  { name :: Name
  , node :: H.ClassP
  }
  deriving (Show, Eq)

data BindDecl = BindDecl
  { name :: Name
  , node :: H.BindP :+ H.FunctionP :+ AST.Nil
  }
  deriving (Show, Eq)

data SigDecl = SigDecl
  { name :: Name
  , node :: H.SignatureP
  }
  deriving (Show, Eq)

data DataFamilyDecl = DataFamilyDecl
  {name :: Name, node :: H.DataFamilyP}
  deriving (Show, Eq)

data NewtypeDecl = NewtypeDecl
  { name :: Name
  , node :: H.NewtypeP
  }
  deriving (Show, Eq)

data PatternSigDecl = PatternSigDecl
  { name :: Name
  , node :: H.SignatureP
  }
  deriving (Show, Eq)

data PatternDecl = PatternDecl
  { name :: Name
  , node :: H.EquationP
  }
  deriving (Show, Eq)

data TypeFamilyDecl = TypeFamilyDecl
  { name :: Name
  , node :: H.TypeFamilyP
  }
  deriving (Show, Eq)

data TypeSynonymDecl = TypeSynonymDecl
  { name :: Name
  , node :: H.TypeSynomymP
  }
  deriving (Show, Eq)

data Decl
  = DeclData DataDecl
  | DeclNewtype NewtypeDecl
  | DeclClass ClassDecl
  | DeclSig SigDecl
  | DeclBind BindDecl
  | DeclDataFamily DataFamilyDecl
  | DeclPatternSig PatternSigDecl
  | DeclPattern PatternDecl
  | DeclTypeFamily TypeFamilyDecl
  | DeclTypeSynonym TypeSynonymDecl
  deriving (Show, Eq)

data Program = Program
  { mod :: Maybe ModuleText
  , imports :: [Import]
  , exports :: Maybe [ExportItem]
  , decls :: [Decl]
  , node :: Haskell.HaskellP
  }
  deriving (Show, Eq)

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

data FunctionBind = FunctionBind
  { fnName :: Text
  , params :: [Param]
  }
  deriving (Show, Eq)

data Params = Params
  { params :: [Param]
  }
  deriving (Show, Eq)

data Param
  = ParamVar Variable
  | ParamWildcard
  | ParamOther
  deriving (Show, Eq)

data Pattern = Pattern
  { patVars :: [Variable]
  }

data Variable = Variable
  { name :: Text
  , dynNode :: DynNode
  }
  deriving (Show, Eq)

data LocalDecls = LocalDecls
  { decls :: [Decl]
  }
