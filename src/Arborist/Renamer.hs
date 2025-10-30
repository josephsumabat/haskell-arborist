{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Arborist.Renamer (
  renamePrg,
  resolvedLocs,
  resolvedNameLocs,
  resolvedConstructorLocs,
  ResolvedVariable (..),
  ResolvedName (..),
  ResolvedConstructor (..),
  RenamePhase,
  Resolveable,
)
where

import AST qualified
import AST.Extension
import AST.Haskell qualified as AST
import Arborist.ProgramIndex
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
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir

data RenamePhase

data ResolvedVariable
  = ResolvedVariable ResolvedVarInfo
  | AmbiguousGlobalVar (NE.NonEmpty GlblVarInfo)
  | AmbiguousLocalVar LocalVarInfo
  | ResolvedField
  | NoVarFound
  deriving (Show)

data ResolvedName
  = ResolvedName
      { nameInfo :: GlblNameInfo
      , nameKind :: NameKind
      }
  | AmbiguousName (NE.NonEmpty (GlblNameInfo))
  | NoNameFound
  deriving (Show, Eq)

data ResolvedConstructor
  = ResolvedConstructor GlblConstructorInfo
  | AmbiguousConstructor (NE.NonEmpty GlblConstructorInfo)
  | NoConstructorFound
  deriving (Show, Eq)

resolvedNameLocs :: ResolvedName -> [(Hir.ModuleText, LineColRange)]
resolvedNameLocs resolvedName =
  case resolvedName of
    ResolvedName nameInfo _ -> [(nameInfo.originatingMod, nameInfo.loc)]
    AmbiguousName names -> NE.toList $ (\name -> (name.originatingMod, name.loc)) <$> names
    NoNameFound -> []

resolvedConstructorLocs :: ResolvedConstructor -> [(Hir.ModuleText, LineColRange)]
resolvedConstructorLocs resolvedCon =
  case resolvedCon of
    ResolvedConstructor conInfo -> [(conInfo.originatingMod, conInfo.loc)]
    AmbiguousConstructor cons -> NE.toList $ (\con -> (con.originatingMod, con.loc)) <$> cons
    NoConstructorFound -> []

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
  type XConstructor RenamePhase = ResolvedConstructor

type HaskellR = AST.Haskell RenamePhase

type Resolveable ext = AST.Name ext AST.:+ AST.Variable ext AST.:+ AST.Constructor ext AST.:+ AST.Nil

-- | Renamer state within a single module
data RenamerEnv = RenamerEnv
  { qualifiedImports :: Set.Set ModNamespace
  , unqualifiedImports :: Set.Set ModNamespace
  , scope :: [Scope]
  }

renamePrg ::
  ProgramIndex ->
  ExportIndex ->
  Hir.Read.Program ->
  Maybe (AST.Haskell RenamePhase)
renamePrg availPrgs exportIdx prg =
  let initialScope = []
      (qualifiedImports, unqualifiedImports) =
        Bifunctor.bimap
          (Set.fromList . fmap getImportModName)
          (Set.fromList . fmap getImportModName)
          (List.partition (\imp -> imp.qualified) (Hir.getImports prg))
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

  getImportModName :: Hir.Read.Import -> ModNamespace
  getImportModName imp = fromMaybe imp.mod imp.alias

  resolveNode :: RenamerEnv -> AST.DynNode -> AST.DynNode
  resolveNode renamerEnv n =
    case AST.cast @(Resolveable ParsePhase) n of
      Just (AST.Inj @(AST.NameP) nameNode) ->
        let resolvedName = resolveName renamerEnv nameNode
         in AST.getDynNode $ AST.modifyNameExt @RenamePhase nameNode (\_ -> resolvedName)
      Just (AST.Inj @(AST.VariableP) node) ->
        let resolvedVar = resolveVar renamerEnv node
            result = AST.getDynNode $ AST.modifyVariableExt @RenamePhase node (\_ -> resolvedVar)
         in result
      Just (AST.Inj @(AST.ConstructorP) conNode) ->
        let resolvedCon = resolveConstructor renamerEnv conNode
         in AST.getDynNode $ AST.modifyConstructorExt @RenamePhase conNode (\_ -> resolvedCon)
      Just _ -> n
      Nothing -> n

  -- get type name + qualifier
  resolveName :: RenamerEnv -> AST.NameP -> ResolvedName
  resolveName renamerEnv nameP =
    case renamerEnv.scope of
      [] -> NoNameFound
      (currScope : _) ->
        case nameP.dynNode.nodeParent >>= AST.cast @AST.QualifiedP >>= (eitherToMaybe . AST.unwrap) of
          Just qualU -> getGlobalResolvedNames renamerEnv currScope nameP.dynNode.nodeText (Just qualU.module')
          Nothing -> getGlobalResolvedNames renamerEnv currScope nameP.dynNode.nodeText Nothing

  -- set as no name, corrrect mapping, or ambigous
  getGlobalResolvedNames :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> ResolvedName
  getGlobalResolvedNames renEnv currScope name qualifier =
    let infos = getValidGlobalNameInfos renEnv currScope name qualifier
     in case infos of
          [] -> NoNameFound
          [x] -> (\info -> ResolvedName info (info.nameKind)) x
          (x : xs) -> AmbiguousName (x NE.:| xs)

  -- find list of possible mappings
  getValidGlobalNameInfos :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> [GlblNameInfo]
  getValidGlobalNameInfos renamerEnv currScope name qualifier =
    case Map.lookup name (currScope.glblNameInfo) of
      Nothing -> []
      Just impNameMap ->
        case qualifier of
          Nothing -> filter (\nameInfo -> not nameInfo.requiresQualifier) $ List.foldl' collectUnqualified [] (Map.toList impNameMap)
          Just q ->
            let mAliasName = (eitherToMaybe . Hir.parseModuleText) q
             in filter
                  ( \nameInfo ->
                      let aliasSet = NESet.map (.namespace) nameInfo.importedFrom
                       in maybe False (\alias -> alias `NESet.member` aliasSet) mAliasName
                  )
                  $ List.foldl' collectQualified [] (Map.toList impNameMap)
       where
        collectQualified acc (_impInfo, nameInfos) = nameInfos ++ acc
        collectUnqualified acc (impInfo, nameInfos)
          | impInfo.namespace `Set.member` renamerEnv.unqualifiedImports = nameInfos ++ acc
          | otherwise = acc

  -- Lookup a var name
  resolveVar :: RenamerEnv -> AST.VariableP -> ResolvedVariable
  resolveVar renamerEnv varP =
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
    case Map.lookup varName currScope.glblVarInfo of
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
                        let aliasSet = NESet.map (.namespace) varInfo.importedFrom
                         in maybe False (\alias -> alias `NESet.member` aliasSet) mAliasName
                    )
                  $ List.foldl' collectQualified [] (Map.toList impVarMap)
   where
    collectUnqualified acc (impInfo, varInfos)
      | impInfo.namespace `Set.member` renamerEnv.unqualifiedImports = varInfos ++ acc
      | otherwise = acc

    collectQualified acc (_impInfo, varInfos) = varInfos ++ acc

  -- same helper functions as for name but for constructors (modularize later?)
  resolveConstructor :: RenamerEnv -> AST.ConstructorP -> ResolvedConstructor
  resolveConstructor renamerEnv conP =
    case renamerEnv.scope of
      [] -> NoConstructorFound
      (currScope : _) ->
        case conP.dynNode.nodeParent >>= AST.cast @AST.QualifiedP >>= (eitherToMaybe . AST.unwrap) of
          Just qualU -> getGlobalResolvedConstructors renamerEnv currScope conP.dynNode.nodeText (Just qualU.module')
          Nothing -> getGlobalResolvedConstructors renamerEnv currScope conP.dynNode.nodeText Nothing

  getGlobalResolvedConstructors :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> ResolvedConstructor
  getGlobalResolvedConstructors renamerEnv currScope conName qualifier =
    let conInfos = getValidGlobalConstructorInfos renamerEnv currScope conName qualifier
     in case conInfos of
          [] -> NoConstructorFound
          [x] -> ResolvedConstructor x
          (x : xs) -> AmbiguousConstructor (x NE.:| xs)

  getValidGlobalConstructorInfos :: RenamerEnv -> Scope -> T.Text -> Maybe AST.ModuleP -> [GlblConstructorInfo]
  getValidGlobalConstructorInfos renamerEnv currScope conName qualifier =
    case Map.lookup conName (currScope.glblConstructorInfo) of
      Nothing -> []
      Just impConMap ->
        case qualifier of
          Nothing ->
            filter (\conInfo -> not conInfo.requiresQualifier) $ List.foldl' collectUnqualified [] (Map.toList impConMap)
          Just q ->
            let mAliasName = (eitherToMaybe . Hir.parseModuleText) q
             in filter
                  ( \conInfo ->
                      let aliasSet = NESet.map (.namespace) conInfo.importedFrom
                       in maybe False (\alias -> alias `NESet.member` aliasSet) mAliasName
                  )
                  $ List.foldl' collectQualified [] (Map.toList impConMap)
   where
    collectQualified acc (_impInfo, conInfos) = conInfos ++ acc
    collectUnqualified acc (impInfo, conInfos)
      | impInfo.namespace `Set.member` renamerEnv.unqualifiedImports = conInfos ++ acc
      | otherwise = acc
