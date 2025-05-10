module ModContext where

import AST.Cast qualified as AST
import AST.Err qualified as AST
import AST.Extension qualified as AST
import AST.Haskell qualified as AST
import AST.Node qualified as AST
import AST.Sum qualified as Sum
import AST.Unwrap qualified as AST
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Hir.Types qualified as Hir
import TreeSitter.Api

-- | Externally visible module information summary
data ModContext = ModContext
  { modName :: Module
  , exports :: [Export]
  , imports :: [Import]
  }
  deriving (Show, Eq)

data Import = Import
  { mod :: Module
  }
  deriving (Show, Eq)

data Export = Export
  { exportName :: Text
  }
  deriving (Show, Eq)

data Module = Module
  { modName :: NE.NonEmpty Text
  }
  deriving (Show, Eq)

data FnDeclaration = FnDeclaration
  { name :: Text
  , declPos :: Maybe Range
  , sigPos :: Maybe Range
  , varNames :: [Text]
  , typeSig :: Maybe Text
  , haddock :: Maybe Text
  }

getModContext :: AST.HaskellP -> AST.Err (Maybe Hir.Decl)
getModContext haskell = do
  haskellU <- AST.unwrap haskell
  let topLevelFunctions = maybe [] ((filterNode @AST.FunctionP) . AST.getDynNode) haskellU.declarations
  undefined
 where
  getImports :: AST.ImportsUP -> Import
  getImports astImport = do
    -- astImport.import'
    undefined

  toImport :: AST.ImportUP -> AST.Err Import
  toImport astImport = do
    modRaw <- AST.unwrap astImport.module'
    pure $
      Import
        { mod = toModule modRaw
        }

  toModule :: AST.ModuleUP -> Module
  toModule modAst = do
    Module
      { modName = (.nodeText) . AST.getDynNode <$> modAst.children
      }

getFnDeclarationInfo :: AST.DeclarationsP -> Map Text FnDeclaration
getFnDeclarationInfo decls = do
  let declNodes = decls.dynNode.nodeChildren
  Map.empty
 where
  go' :: [AST.DynNode] -> [AST.DynNode] -> Map Text FnDeclaration -> Map Text FnDeclaration
  go' [] _ !prevMap = prevMap
  -- go' (currNode:nodesToProcess) [] !prevMap =
  go' (currNode : nodesToProcess) (prevNode : xs) !prevMap =
    undefined

  updateHaddockInfo :: AST.Haddock AST.ParsePhase -> FnDeclaration -> FnDeclaration
  updateHaddockInfo haddockNode fnDecl =
    let haddockText = haddockNode.dynNode.nodeText
     in fnDecl {haddock = Just haddockText}

  updateFnTypeSig :: AST.SignatureU AST.ParsePhase -> FnDeclaration -> FnDeclaration
  updateFnTypeSig sig fnDecl =
    let mTypeText = (.dynNode.nodeText) <$> sig.type'
     in fnDecl {typeSig = mTypeText}

  updateFn :: AST.FunctionU AST.ParsePhase -> FnDeclaration -> FnDeclaration
  updateFn fnAst fnDecl =
    let mfnName = handleFnName =<< fnAst.name
        fnPos = fnAst.dynNode.nodeRange
     in undefined

  handleFnName :: (AST.PrefixIdP Sum.:+ AST.VariableP Sum.:+ Sum.Nil) -> Maybe Text
  handleFnName (Sum.X prefixIdAst@(AST.PrefixId _ _)) = Just prefixIdAst.dynNode.nodeText
  handleName (Sum.Rest (Sum.X varAst@(AST.Variable {}))) = Just varAst.dynNode.nodeText
  handleName _ = Nothing

  initialFnDeclaration fnName =
    FnDeclaration
      { name = fnName
      , declPos = Nothing
      , sigPos = Nothing
      , varNames = []
      , typeSig = Nothing
      , haddock = Nothing
      }

filterNode :: (AST.Cast n) => AST.DynNode -> [n]
filterNode node =
  mapMaybe AST.cast node.nodeChildren

fn :: Int
fn = 1
