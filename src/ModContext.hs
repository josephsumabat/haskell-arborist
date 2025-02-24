module ModContext where

import qualified AST.Haskell as AST
import qualified AST.Unwrap as AST
import qualified AST.Err as AST
import qualified AST.Node as AST
import qualified AST.Cast as AST
import qualified AST.Sum as Sum
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import TreeSitter.Api
import Data.Maybe
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir

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
  {
    name :: Text
  , declPos :: Maybe Range
  , sigPos :: Maybe Range
  , varNames :: [Text]
  , typeSig :: Maybe Text
  , haddock :: Maybe Text
  }

getModContext :: AST.Haskell -> AST.Err (Maybe Hir.Decl)
getModContext haskell = do
  haskellU <- AST.unwrap haskell
  let topLevelFunctions = maybe [] ((filterNode @AST.Function) . AST.getDynNode) haskellU.declarations
  undefined
    where

      getImports :: AST.ImportsU -> Import
      getImports astImport = do
        -- astImport.import'
        undefined

      toImport :: AST.ImportU -> AST.Err Import
      toImport astImport = do
        modRaw <- AST.unwrap astImport.module'
        pure $ Import
          {
            mod = toModule modRaw
          }
      
      toModule :: AST.ModuleU -> Module
      toModule modAst = do
        Module 
          {
            modName = (.nodeText) . AST.getDynNode <$> modAst.children
          }

getFnDeclarationInfo :: AST.Declarations -> Map Text FnDeclaration
getFnDeclarationInfo decls = do
  let declNodes = decls.dynNode.nodeChildren
  Map.empty
    where
      go' :: [AST.DynNode] -> [AST.DynNode] -> Map Text FnDeclaration -> Map Text FnDeclaration
      go' [] _ !prevMap = prevMap
      -- go' (currNode:nodesToProcess) [] !prevMap =
      go' (currNode:nodesToProcess) (prevNode:xs) !prevMap =
        undefined
             
      updateHaddockInfo :: AST.Haddock -> FnDeclaration -> FnDeclaration
      updateHaddockInfo haddockNode fnDecl = 
        let haddockText = haddockNode.dynNode.nodeText in
            fnDecl { haddock = Just haddockText }

      updateFnTypeSig :: AST.SignatureU -> FnDeclaration -> FnDeclaration
      updateFnTypeSig sig fnDecl =
        let mTypeText = (.dynNode.nodeText) <$> sig.type' in
            fnDecl { typeSig = mTypeText }

      updateFn :: AST.FunctionU -> FnDeclaration -> FnDeclaration
      updateFn fnAst fnDecl = 
        let mfnName = handleFnName =<< fnAst.name
            fnPos = fnAst.dynNode.nodeRange in
        undefined

      handleFnName :: (AST.PrefixId Sum.:+ AST.Variable Sum.:+ Sum.Nil) -> Maybe Text
      handleFnName (Sum.X prefixIdAst@(AST.PrefixId _ _)) = Just prefixIdAst.dynNode.nodeText
      handleName (Sum.Rest (Sum.X varAst@(AST.Variable _))) = Just varAst.dynNode.nodeText
      handleName _ = Nothing
      

      initialFnDeclaration fnName =
        FnDeclaration
          {
            name = fnName
          , declPos = Nothing
          , sigPos = Nothing
          , varNames = []
          , typeSig  = Nothing
          , haddock  = Nothing
          }


filterNode :: (AST.Cast n) => AST.DynNode -> [n]
filterNode node =
  mapMaybe AST.cast node.nodeChildren

fn :: Int
fn = 1
