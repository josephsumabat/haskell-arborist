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
import Arborist.Exports
import Arborist.ModGraph
import Arborist.Scope
import Arborist.Scope.Global
import Arborist.Scope.Types
import Control.Error
import Data.Bifunctor qualified as Bifunctor
import Data.Either.Extra (eitherToMaybe)
import Data.HashMap.Lazy qualified as Map
import Data.LineColRange
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as T
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import Arborist.Debug.Trace

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

instance AST.NodeX RenamePhase where
  type XName RenamePhase = ResolvedName
  type XVariable RenamePhase = ResolvedVariable

type HaskellR = AST.Haskell RenamePhase

type Resolveable = AST.NameP AST.:+ AST.VariableP AST.:+ AST.Nil

-- | Renamer state within a single module
data RenamerEnv = RenamerEnv
  { qualifiedImports :: Set.Set ModNamespace
  , unqualifiedImports :: Set.Set ModNamespace
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
   in AST.cast @HaskellR (go renamerEnv prg.node.dynNode)
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
        let (_, reversedChildren) =
              List.foldl' resolveDoChild (renamerEnv, []) n.nodeChildren
            newChildren = reverse reversedChildren
         in (resolveNode renamerEnv n) {AST.nodeChildren = newChildren}

  -- Special case for do notation
  -- we prepend in reverse order for efficiency - up to the caller to reverse
  resolveDoChild :: (RenamerEnv, [AST.DynNode]) -> AST.DynNode -> (RenamerEnv, [AST.DynNode])
  resolveDoChild (renamerEnv, renamedChildren) n =
    let !newScope = getScope availPrgs exportIdx n renamerEnv.scope
        !newRenamerEnv = renamerEnv {scope = newScope}
     in (newRenamerEnv, go renamerEnv n : renamedChildren)

  getImportModName :: Hir.Import -> ModNamespace
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
    case renamerEnv.scope of
      [] -> NoVarFound
      (currScope : _) -> handle currScope
   where
    fieldParent = varP.dynNode.nodeParent >>= AST.cast @AST.FieldNameP >>= (eitherToMaybe . AST.unwrap)
    qualifiedParent = varP.dynNode.nodeParent >>= AST.cast @AST.QualifiedP >>= (eitherToMaybe . AST.unwrap)

    handle :: Scope -> ResolvedVariable
    handle currScope
      -- The variable is a field - we dont have enough information to figure out where it is from right now (would require typechecking)
      | Just _f <- fieldParent = ResolvedField
      -- Qualified variable
      | Just qualU <- qualifiedParent =
          let qualifier = qualU.module'
              varName = varP.dynNode.nodeText
              globalResolved = getGlobalResolvedVariables renamerEnv currScope varName (Just qualifier)
           in globalResolved
      | otherwise =
          let varName = varP.dynNode.nodeText
              localResolved = getLocalResolvedVariables currScope varName
              globalResolved = getGlobalResolvedVariables renamerEnv currScope varName Nothing
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

  getGlobalResolvedVariables :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> ResolvedVariable
  getGlobalResolvedVariables renamerEnv currScope varName qualifier =
    let varInfos = getValidGlobalVarInfos renamerEnv currScope varName qualifier
     in case varInfos of
          [] -> NoVarFound
          [x] -> ResolvedVariable (ResolvedGlobal x)
          (x : xs) -> AmbiguousGlobalVar (x NE.:| xs)

  getValidGlobalVarInfos :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> [GlblVarInfo]
  getValidGlobalVarInfos renamerEnv currScope varName qualifier =
    case Map.lookup varName (currScope.glblVarInfo) of
      Nothing -> []
      Just impVarMap ->
        case qualifier of
          Nothing ->
            tryMergeGlblVarInfo $
              filter (\varInfo -> not varInfo.requiresQualifier) $
                List.foldl' collectUnqualified [] (Map.toList impVarMap)
          Just q ->
            let mAliasName = (eitherToMaybe . Hir.parseModuleText) q
             in tryMergeGlblVarInfo
                  $ filter
                    ( \varInfo ->
                        let aliasSet = NESet.map (.namespace) varInfo.importedFrom in
                        maybe False (\alias -> alias `NESet.member` aliasSet) mAliasName
                    )
                  $ List.foldl' collectQualified [] (Map.toList impVarMap)
   where
    collectUnqualified acc (impInfo, varInfos)
      | impInfo.namespace `Set.member` renamerEnv.unqualifiedImports = varInfos ++ acc
      | otherwise = acc

    collectQualified acc (_impInfo, varInfos) = varInfos ++ acc
