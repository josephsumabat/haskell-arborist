{-# LANGUAGE TypeFamilies #-}
module Arborist.Renamer where

import AST.Extension
import AST.Haskell
import qualified AST
import AST.Haskell.Generated qualified as AST
import qualified Data.Map.Lazy as Map
import Data.LineColRange
import qualified Data.Text as T
import Debug.Trace
import qualified Hir.Parse as Hir
import Arborist.Scope

data RenamePhase

data ResolvedName = ResolvedName | AmbiguousName
  deriving Show

-- type instance XName RenamePhase = ResolvedName
instance NodeX RenamePhase where
  type XName RenamePhase = ResolvedName
  type XVariable RenamePhase = ResolvedName

type HaskellR = Haskell RenamePhase

data ScopeData =
  ScopeData
    {
      bindingName :: T.Text
    , loc :: LineColRange
    }

type Rib = Scope

type Resolveable = NameP AST.:+ VariableP AST.:+ AST.Nil

type ScopeChanger = HaskellP AST.:+ AST.Nil

getScope :: AST.DynNode -> [Rib] -> [Rib]
getScope n curScope = 
        case AST.cast @ScopeChanger n of
          Just (AST.Inj @(HaskellP) haskellNode) ->
            let (_, prg) = Hir.parseHaskell haskellNode in
                curScope
          Just _ -> curScope
          Nothing -> curScope

renameHaskell :: Haskell ParsePhase -> Maybe (Haskell RenamePhase)
renameHaskell haskell =
  let initialScope = [] in
    AST.cast @HaskellR (go initialScope haskell.dynNode)
    where
      go :: [Rib] -> AST.DynNode -> AST.DynNode
      go scope n = 
        let !newScope = getScope n scope
            !newNode = go newScope <$> n.nodeChildren
              in 
        (resolveNode n) { AST.nodeChildren = newNode } 
      
      resolveNode :: AST.DynNode -> AST.DynNode
      resolveNode n =
        case AST.cast @Resolveable n of
          Just (AST.Inj @(NameP) nameNode) ->
              AST.getDynNode $ AST.modifyNameExt @RenamePhase nameNode (\_ -> ResolvedName )
          Just (AST.Inj @(VariableP) node) ->
              AST.getDynNode $ AST.modifyVariableExt @RenamePhase node (\_ -> ResolvedName )
          Just _ -> n
          Nothing -> n

      resolveName :: T.Text -> Scope -> ResolvedName
      resolveName = undefined

