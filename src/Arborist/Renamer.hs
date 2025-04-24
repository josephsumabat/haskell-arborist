{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Arborist.Renamer (
  renamePrg,
  resolvedLocs,
  ResolvedVariable (..),
  ResolvedName (..),
  RenamePhase,
)
where

import AST qualified
import AST.Haskell qualified as AST
import Arborist.ModGraph
import Arborist.Scope
import Arborist.Scope.Global
import Arborist.Scope.Types
import Control.Error
import Data.Bifunctor qualified as Bifunctor
import Data.HashMap.Lazy qualified as Map
import Data.LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import Hir.Types qualified as Hir

data RenamePhase

data ResolvedVariable
  = ResolvedVariable ResolvedVarInfo
  | AmbiguousGlobalVar (NE.NonEmpty GlblVarInfo)
  | AmbiguousLocalVar LocalVarInfo
  | ResolvedField
  | NoVarFound
  deriving (Show)

resolvedLocs :: Hir.ModuleText -> ResolvedVariable -> [(Hir.ModuleText, LineColRange)]
resolvedLocs thisMod resolvedVar =
  case resolvedVar of
    ResolvedVariable resolvedVarInfo ->
      case resolvedVarInfo of
        ResolvedGlobal glblVarInfo ->
          [(glblVarInfo.originatingMod, glblVarInfo.loc)]
        ResolvedLocal lclVarInfo ->
          [(thisMod, resolvedLclVarToLoc lclVarInfo)]
    AmbiguousGlobalVar glblVars ->
      NE.toList $ (\glblVar -> (glblVar.originatingMod, glblVar.loc)) <$> glblVars
    AmbiguousLocalVar lclVars -> (thisMod,) <$> (NE.toList $ lclVarInfoToLoc lclVars)
    ResolvedField -> []
    NoVarFound -> []

data ResolvedName
  = NoNameFound
  deriving (Show)

fstResolved :: [ResolvedVariable] -> ResolvedVariable
fstResolved resolvedNames =
  List.foldl' moreResolved NoVarFound resolvedNames
 where
  moreResolved :: ResolvedVariable -> ResolvedVariable -> ResolvedVariable
  moreResolved NoVarFound a = a
  moreResolved a NoVarFound = a
  moreResolved a@(ResolvedVariable _) _ = a
  moreResolved _ a@(ResolvedVariable _) = a
  moreResolved a _b = a

-- type instance XName RenamePhase = ResolvedVariable
instance AST.NodeX RenamePhase where
  type XName RenamePhase = ResolvedName
  type XVariable RenamePhase = ResolvedVariable

type HaskellR = AST.Haskell RenamePhase

type Resolveable = AST.NameP AST.:+ AST.VariableP AST.:+ AST.Nil

data RenamerEnv = RenamerEnv
  { qualifiedImports :: Set.Set Hir.ModuleText
  , unqualifiedImports :: Set.Set Hir.ModuleText
  , scope :: [Scope]
  }
  deriving (Show)

renamePrg ::
  ProgramIndex ->
  ExportIndex ->
  Hir.Program ->
  Maybe (AST.Haskell RenamePhase)
renamePrg availPrgs exportIdx prg =
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
        let !newScope = getScope availPrgs exportIdx n renamerEnv.scope
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
    let !newScope = getScope availPrgs exportIdx n renamerEnv.scope
        !newRenamerEnv = renamerEnv {scope = newScope}
     in -- traceShowPretty (head newScope).lclVarInfo $
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
            result = AST.getDynNode $ AST.modifyVariableExt @RenamePhase node (\_ -> resolvedName)
         in result
      Just _ -> n
      Nothing -> n

  -- Lookup a var name
  resolveVarName :: RenamerEnv -> AST.VariableP -> ResolvedVariable
  resolveVarName renamerEnv varP =
    let varName = varP.dynNode.nodeText
        fieldParent = AST.cast @AST.FieldNameP =<< varP.dynNode.nodeParent
     in case fieldParent of
          -- The variable is a field - we dont have enough information to figure out where it is from right now (would require typechecking)
          Just _f -> ResolvedField
          Nothing ->
            case renamerEnv.scope of
              [] -> NoVarFound
              (currScope : _) ->
                let localResolved = getLocalResolvedVariables currScope varName
                    globalResolved = getGlobalResolvedVariables renamerEnv currScope varName
                 in fstResolved [localResolved, globalResolved]

  getLocalResolvedVariables :: Scope -> T.Text -> ResolvedVariable
  getLocalResolvedVariables currScope varName =
    case Map.lookup varName (currScope.lclVarInfo) of
      Just varInfo@(LocalVarParam l) -> case l of
        (v NE.:| []) -> ResolvedVariable (ResolvedLocal (VarInfoParam v))
        _ -> AmbiguousLocalVar varInfo
      Just varInfo@(LocalVarLet l) -> case l of
        (v NE.:| []) -> ResolvedVariable (ResolvedLocal (VarInfoLet v))
        _ -> AmbiguousLocalVar varInfo
      Just varInfo@(LocalVarWhere l) -> case l of
        (v NE.:| []) -> ResolvedVariable (ResolvedLocal (VarInfoWhere v))
        _ -> AmbiguousLocalVar varInfo
      Just varInfo@(LocalVarBind l) -> case l of
        (v NE.:| []) -> ResolvedVariable (ResolvedLocal (VarInfoBind v))
        _ -> AmbiguousLocalVar varInfo
      Nothing -> NoVarFound

  getGlobalResolvedVariables :: RenamerEnv -> Scope -> T.Text -> ResolvedVariable
  getGlobalResolvedVariables renamerEnv currScope varName =
    let varInfos = getValidGlobalVarInfos renamerEnv currScope varName
     in case varInfos of
          [] -> NoVarFound
          [x] -> ResolvedVariable (ResolvedGlobal x)
          (x : xs) -> AmbiguousGlobalVar (x NE.:| xs)

  getValidGlobalVarInfos :: RenamerEnv -> Scope -> T.Text -> [GlblVarInfo]
  getValidGlobalVarInfos renamerEnv currScope varName =
    case Map.lookup varName (currScope.glblVarInfo) of
      Nothing -> []
      Just modVarMap ->
        tryMergeGlblVarInfo $
          filter (\varInfo -> not varInfo.requiresQualifier) $
            List.foldl' collect [] (Map.toList modVarMap)
   where
    collect acc (modName, varInfos)
      | modName `Set.member` renamerEnv.unqualifiedImports = varInfos ++ acc
      | otherwise = acc
