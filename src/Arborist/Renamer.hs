{-# LANGUAGE TypeFamilies #-}

module Arborist.Renamer where

import AST qualified
import AST.Haskell qualified as AST
import Arborist.Scope
import Arborist.Scope.Types
import Data.Bifunctor qualified as Bifunctor
import Data.HashMap.Lazy qualified as Map
import Data.LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Hir.Types qualified as Hir
import Debug.Trace
import qualified Data.Text.Lazy as Text
import Text.Pretty.Simple

traceShowPretty :: Show s => s -> a -> a
traceShowPretty p v = trace (Text.unpack . pShowNoColor $ p) v

data RenamePhase

data ResolvedName
  = ResolvedName VarInfo
  | AmbiguousGlobalName (NE.NonEmpty GlblVarInfo)
  | AmbiguousLocalName LocalVarInfo
  | NoNameFound
  deriving (Show)

fstResolved :: [ResolvedName] -> ResolvedName
fstResolved resolvedNames =
  List.foldl' moreResolved NoNameFound resolvedNames
 where
  moreResolved :: ResolvedName -> ResolvedName -> ResolvedName
  moreResolved NoNameFound a = a
  moreResolved a NoNameFound = a
  moreResolved a@(ResolvedName _) _ = a
  moreResolved _ a@(ResolvedName _) = a
  moreResolved a _b = a

-- type instance XName RenamePhase = ResolvedName
instance AST.NodeX RenamePhase where
  type XName RenamePhase = ResolvedName
  type XVariable RenamePhase = ResolvedName

type HaskellR = AST.Haskell RenamePhase

type Resolveable = AST.NameP AST.:+ AST.VariableP AST.:+ AST.Nil

data RenamerEnv = RenamerEnv
  { qualifiedImports :: Set.Set Hir.ModuleText
  , unqualifiedImports :: Set.Set Hir.ModuleText
  , scope :: [Scope]
  }
  deriving (Show)

renamePrg :: ProgramIndex -> Hir.Program -> Maybe (AST.Haskell RenamePhase)
renamePrg availPrgs prg =
  let initialScope = []
      (qualifiedImports, unqualifiedImports) =
        Bifunctor.bimap
          (Set.fromList . fmap getImportModName)
          (Set.fromList . fmap getImportModName)
          (List.partition (\imp -> imp.qualified) prg.imports)
      renamerEnv =
        RenamerEnv
          { qualifiedImports
          , unqualifiedImports = maybe unqualifiedImports (`Set.insert` unqualifiedImports) prg.mod
          , scope = initialScope
          }
   in AST.cast @HaskellR (go renamerEnv prg.dynNode)
 where
  go :: RenamerEnv -> AST.DynNode -> AST.DynNode
  go renamerEnv n =
    case AST.cast @AST.DoP n of
      Nothing ->
        let !newScope = getScope availPrgs n renamerEnv.scope
            !newRenamerEnv = renamerEnv {scope = newScope}
            !newChildren = go newRenamerEnv <$> n.nodeChildren
         in (resolveNode renamerEnv n) {AST.nodeChildren = newChildren}
      -- do node is a special case since statements are in scope for all
      -- subsequent statements
      Just _doNode ->
        let (_, reversedNewChildren) =
              List.foldl' resolveDoChild (renamerEnv, []) n.nodeChildren
            newChildren = reverse reversedNewChildren
         in (resolveNode renamerEnv n) {AST.nodeChildren = newChildren}

  -- Special case for do notation
  -- up to the caller to reverse
  resolveDoChild :: (RenamerEnv, [AST.DynNode]) -> AST.DynNode -> (RenamerEnv, [AST.DynNode])
  resolveDoChild (renamerEnv, renamedChildren) n =
    let !newScope = getScope availPrgs n renamerEnv.scope
        !newRenamerEnv = renamerEnv {scope = newScope}
     in
     --traceShowPretty (head newScope).lclVarInfo $
       (newRenamerEnv, go renamerEnv n : renamedChildren)

  getImportModName :: Hir.Import -> Hir.ModuleText
  getImportModName imp = fromMaybe imp.mod imp.alias

  resolveNode :: RenamerEnv -> AST.DynNode -> AST.DynNode
  resolveNode renamerEnv n =
    case AST.cast @Resolveable n of
      Just (AST.Inj @(AST.NameP) nameNode) ->
        AST.getDynNode $ AST.modifyNameExt @RenamePhase nameNode (\_ -> NoNameFound)
      Just (AST.Inj @(AST.VariableP) node) ->
        let resolvedName = resolveVarName renamerEnv node
         in AST.getDynNode $ AST.modifyVariableExt @RenamePhase node (\_ -> resolvedName)
      Just _ -> n
      Nothing -> n

  -- Lookup a var name
  resolveVarName :: RenamerEnv -> AST.VariableP -> ResolvedName
  resolveVarName renamerEnv varP =
    let varName = varP.dynNode.nodeText
     in case renamerEnv.scope of
          [] -> NoNameFound
          (currScope : _) ->
            let localResolved = getLocalResolvedNames currScope varName
                globalResolved = getGlobalResolvedNames renamerEnv currScope varName
             in fstResolved [localResolved, globalResolved]

  getLocalResolvedNames :: Scope -> T.Text -> ResolvedName
  getLocalResolvedNames currScope varName =
    case Map.lookup varName (currScope.lclVarInfo) of
      Just varInfo@(LocalVarParam l) -> case l of
        (v NE.:| []) -> ResolvedName (VarInfoParam v)
        _ -> AmbiguousLocalName varInfo
      Just varInfo@(LocalVarLet l) -> case l of
        (v NE.:| []) -> ResolvedName (VarInfoLet v)
        _ -> AmbiguousLocalName varInfo
      Just varInfo@(LocalVarWhere l) -> case l of
        (v NE.:| []) -> ResolvedName (VarInfoWhere v)
        _ -> AmbiguousLocalName varInfo
      Just varInfo@(LocalVarBind l) -> case l of
        (v NE.:| []) -> ResolvedName (VarInfoBind  v)
        _ -> AmbiguousLocalName varInfo
      Nothing -> NoNameFound

  getGlobalResolvedNames :: RenamerEnv -> Scope -> T.Text -> ResolvedName
  getGlobalResolvedNames renamerEnv currScope varName =
    let varInfos = getValidGlobalVarInfos renamerEnv currScope varName
     in case varInfos of
          [] -> NoNameFound
          [x] -> ResolvedName (VarInfoGlobal x)
          (x : xs) -> AmbiguousGlobalName (x NE.:| xs)

  getValidGlobalVarInfos :: RenamerEnv -> Scope -> T.Text -> [GlblVarInfo]
  getValidGlobalVarInfos renamerEnv currScope varName =
    case Map.lookup varName (currScope.glblVarInfo) of
      Nothing -> []
      Just modVarMap ->
        concat
          [ varInfos
          | (modName, varInfos) <- Map.toList modVarMap
          , modName `Set.member` renamerEnv.unqualifiedImports
          ]
