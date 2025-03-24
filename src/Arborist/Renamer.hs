{-# LANGUAGE TypeFamilies #-}

module Arborist.Renamer where

import AST qualified
import AST.Extension
import AST.Haskell qualified as AST
import AST.Haskell.Generated qualified as AST
import Arborist.Scope
import Control.Applicative (Alternative (..))
import Data.Bifunctor qualified as Bifunctor
import Data.HashMap.Lazy qualified as Map
import Data.LineColRange
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as Text
import Debug.Trace
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import Text.Pretty.Simple

data RenamePhase

data ResolvedName
  = ResolvedName VarInfo
  | AmbiguousName [GlblVarInfo]
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

data ScopeData = ScopeData
  { bindingName :: T.Text
  , loc :: LineColRange
  }

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
    let !newScope = getScope availPrgs n renamerEnv.scope
        !newRenamerEnv = renamerEnv {scope = newScope}
        !newNode = go newRenamerEnv <$> n.nodeChildren
     in (resolveNode renamerEnv n) {AST.nodeChildren = newNode}

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
      Nothing -> NoNameFound
      Just v -> ResolvedName (VarInfoLocal v)

  getGlobalResolvedNames :: RenamerEnv -> Scope -> T.Text -> ResolvedName
  getGlobalResolvedNames renamerEnv currScope varName =
    let varInfos = getValidGlobalVarInfos renamerEnv currScope varName
     in case varInfos of
          [] -> NoNameFound
          [x] -> ResolvedName (VarInfoGlobal x)
          xs -> AmbiguousName (xs)

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
