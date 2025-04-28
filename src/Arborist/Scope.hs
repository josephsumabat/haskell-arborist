{-# LANGUAGE TupleSections #-}

module Arborist.Scope where

import AST
import AST.Haskell qualified as AST
import Arborist.ModGraph
import Arborist.Scope.Global
import Arborist.Scope.Local
import Arborist.Scope.Types
import Control.Error (headMay)
import Data.Either.Extra
import Data.List qualified as List
import Data.Maybe
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir

getScope :: ProgramIndex -> ExportIndex -> AST.DynNode -> [Scope] -> [Scope]
getScope availPrgs exportIdx n !scopeStack =
  let currScope = fromMaybe emptyScope (headMay scopeStack)
   in case AST.cast @ScopeChanger n of
        Just (AST.Inj @(AST.HaskellP) haskellNode) ->
          -- Add top level and imported bindings
          let (_, prg) = Hir.parseHaskell haskellNode
              availableNames = getGlobalAvailableNames availPrgs exportIdx prg
              modScope = globalNamesToScope availableNames
           in modScope : scopeStack
        -- import lists do not use the global program scope
        Just (AST.Inj @(AST.ImportP) _importNode) ->
          emptyScope : scopeStack
        Just (AST.Inj @(AST.FunctionP) fnNode) ->
          -- Add local params when encountering a function node
          let params = (Hir.parseFunction fnNode).params
              mWhereBinds = (eitherToMaybe $ AST.unwrap fnNode) >>= (.binds)
              curScope = fromMaybe emptyScope (headMay scopeStack)
              scopeWithParams =
                List.foldl' addParam curScope params
              scopeWithBinds =
                case mWhereBinds of
                  Nothing -> scopeWithParams : scopeStack
                  Just localBinds -> addLocalWhereBinds scopeWithParams (Hir.parseLocalBinds localBinds) : scopeStack
           in scopeWithBinds
        Just (AST.Inj @(AST.BindP) bindNode) ->
          let mBindU = eitherToMaybe $ AST.unwrap bindNode
              mArrow = mBindU >>= (.arrow)
           in case mArrow of
                Nothing ->
                  let mWhereBinds = mBindU >>= (.binds)
                      curScope = fromMaybe emptyScope (headMay scopeStack)
                      scopeWithBinds =
                        case mWhereBinds of
                          Nothing -> scopeStack
                          Just localBinds -> addLocalWhereBinds curScope (Hir.parseLocalBinds localBinds) : scopeStack
                   in scopeWithBinds
                Just _ ->
                  -- bind within do
                  let mRawPats = (eitherToMaybe $ AST.unwrap bindNode) >>= (.pattern')
                   in case mRawPats of
                        Just (AST.Inj @AST.PatternP patNode) -> addLocalPatVars currScope (Hir.parsePattern patNode) : scopeStack
                        Just _ -> scopeStack
                        Nothing -> scopeStack
        Just (AST.Inj @(AST.LetInP) letInNode) ->
          let mLocalBinds = (eitherToMaybe $ AST.unwrap letInNode) >>= (.binds)
           in case mLocalBinds of
                Nothing -> scopeStack
                Just localBinds -> addLocalLetBinds currScope (Hir.parseLocalBinds localBinds) : scopeStack
        -- let binding within do
        Just (AST.Inj @(AST.LetP) letNode) ->
          let mLocalBinds = (eitherToMaybe $ AST.unwrap letNode) >>= (.binds)
           in case mLocalBinds of
                Nothing -> scopeStack
                Just localBinds -> addLocalLetBinds currScope (Hir.parseLocalBinds localBinds) : scopeStack
        Just (AST.Inj @(AST.AlternativeP) caseAltNode) ->
          let mRawPats = (eitherToMaybe $ AST.unwrap caseAltNode) >>= (.pattern')
           in case mRawPats of
                Just (AST.Inj @AST.PatternP patNode) -> addLocalPatVars currScope (Hir.parsePattern patNode) : scopeStack
                Just _ -> scopeStack
                Nothing -> scopeStack
        Just (AST.Inj @(AST.LambdaP) lambdaNode) ->
          let mPats = (.patterns) <$> (eitherToMaybe $ AST.unwrap lambdaNode)
              pats = maybe [] Hir.parsePatterns mPats
           in addManyLocalPatVars currScope pats : scopeStack
        Just _ -> scopeStack
        Nothing -> scopeStack
