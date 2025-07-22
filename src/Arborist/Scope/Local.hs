module Arborist.Scope.Local (
  addManyLocalPatVars,
  addLocalLetBinds,
  addLocalWhereBinds,
  addParam,
  addLocalPatVars,
)
where

import AST
import Arborist.Scope.Types
import Control.Error (headMay)
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Hir
import Hir.Types qualified as Hir

addManyLocalPatVars :: Scope -> [Hir.Pattern] -> Scope
addManyLocalPatVars currScope pats =
  List.foldl'
    addLocalPatVars
    currScope
    (reverse pats) -- Reverse for efficiency

addLocalPatVars :: Scope -> Hir.Pattern -> Scope
addLocalPatVars currScope pat =
  let clearedScope = clearPrevLocalNames currScope (Hir.nameText . (.name) <$> pat.patVars)
   in List.foldl'
        (addLocalBind)
        clearedScope
        (reverse pat.patVars) -- Reverse for efficiency

addLocalLetBinds :: Scope -> Hir.LocalDecls -> Scope
addLocalLetBinds currScope localBinds =
  let clearedScope = clearPrevLocalNames currScope (declNameText <$> localBinds.decls)
   in List.foldl' (addLocalLetDecl) clearedScope localBinds.decls

addLocalWhereBinds :: Scope -> Hir.LocalDecls -> Scope
addLocalWhereBinds currScope localBinds =
  let clearedScope = clearPrevLocalNames currScope (declNameText <$> localBinds.decls)
   in List.foldl' (addLocalWhereDecl) clearedScope localBinds.decls

addLocalBind :: Scope -> Hir.Variable -> Scope
addLocalBind currScope var =
  let name = var.dynNode.nodeText
      mVarInfo = Map.lookup name currScope.lclVarInfo
      existing =
        case mVarInfo of
          Just (LocalVarBind l) -> NE.toList l
          Nothing -> []
          Just _ -> []
      -- TODO: reverse at end
      newInfo = LocalVarBind $ LocalBind var.dynNode NE.:| existing
   in insertScope currScope name newInfo

addLocalWhereDecl :: Scope -> Hir.Decl -> Scope
addLocalWhereDecl currScope localDecl =
  let name = (declName localDecl).node.nodeText
      mVarInfo = Map.lookup name currScope.lclVarInfo
      existing =
        case mVarInfo of
          Just (LocalVarWhere l) -> NE.toList l
          Nothing -> []
          Just _ -> []
      mNewInfo = LocalVarWhere <$> (getNewLocalDecl existing localDecl)
   in case mNewInfo of
        Just newInfo -> insertScope currScope name newInfo
        Nothing -> currScope

addLocalLetDecl :: Scope -> Hir.Decl -> Scope
addLocalLetDecl currScope localDecl =
  let name = (declName localDecl).node.nodeText
      mVarInfo = Map.lookup name currScope.lclVarInfo
      existing =
        case mVarInfo of
          Just (LocalVarLet l) -> NE.toList l
          Just _ -> []
          Nothing -> []
      mNewInfo = LocalVarLet <$> (getNewLocalDecl existing localDecl)
   in case mNewInfo of
        Just newInfo -> insertScope currScope name newInfo
        Nothing -> currScope

insertScope :: Scope -> T.Text -> LocalVarInfo -> Scope
insertScope currScope name newInfo =
  currScope {lclVarInfo = Map.insert name newInfo currScope.lclVarInfo}

clearPrevLocalNames :: Scope -> [T.Text] -> Scope
clearPrevLocalNames scope names =
  List.foldl'
    ( \currScope name ->
        currScope
          { lclVarInfo = Map.delete name currScope.lclVarInfo
          }
    )
    scope
    names

getNewLocalDecl :: [LocalDecl] -> Hir.Decl -> Maybe (NE.NonEmpty LocalDecl)
getNewLocalDecl existing decl =
  let
    (newEntry, rest) = case decl of
      Hir.DeclBind b -> tryMergeBind b existing
      Hir.DeclSig s -> tryMergeSig s existing
      _ -> (Nothing, existing)

    mNewLclVarInfo =
      case (newEntry, rest) of
        (Nothing, (x : xs)) -> Just $ x NE.:| xs
        (Just v, xs) -> Just (v NE.:| xs)
        (Nothing, []) -> Nothing
   in
    mNewLclVarInfo
 where
  -- Note for binds we use the name location since a single raw bind
  -- can have multiple names bound
  tryMergeBind :: Hir.BindDecl -> [LocalDecl] -> (Maybe LocalDecl, [LocalDecl])
  tryMergeBind b [] =
    (Just
      (LocalDecl
        {sig = Nothing, binds = [b]
        , loc = (AST.getDynNode b.name.node).nodeLineColRange
        }), [])
  tryMergeBind b (v : vs) =
    case v.binds of
      [] ->
        let merged :: LocalDecl
            merged = setBind b v
         in (Just merged, vs)
      b : _rest ->
        let (result, rest) = tryMergeBind b vs
         in (result, v : rest)

  setBind :: Hir.BindDecl -> LocalDecl -> LocalDecl
  setBind b v =
    LocalDecl
      { binds = [b]
      , sig = v.sig
      , loc = (AST.getDynNode b.name.node).nodeLineColRange
      }

  tryMergeSig :: Hir.SigDecl -> [LocalDecl] -> (Maybe LocalDecl, [LocalDecl])
  tryMergeSig s [] =
    (Just (LocalDecl {sig = Just s, binds = [], loc = (AST.getDynNode s.node).nodeLineColRange}), [])
  tryMergeSig s (v : vs) =
    case v.sig of
      Nothing ->
        let merged = setSig s v
         in (Just merged, vs)
      Just _ ->
        let (result, rest) = tryMergeSig s vs
         in (result, v : rest)

  setSig :: Hir.SigDecl -> LocalDecl -> LocalDecl
  setSig s v =
    LocalDecl
      { binds = v.binds
      , sig = Just s
      , loc =
          case headMay v.binds of
            Nothing -> (AST.getDynNode s.node).nodeLineColRange
            Just b -> (AST.getDynNode b.node).nodeLineColRange
      }

addParam :: Scope -> Hir.Param -> Scope
addParam scope param =
  case param of
    Hir.ParamVar var ->
      let lclVarInfo =
            LocalParam
              { var = var
              }
          lclVarName = Hir.nameText var.name
       in case Map.lookup lclVarName scope.lclVarInfo of
            Just (LocalVarParam p) ->
              scope
                { lclVarInfo =
                    Map.insert
                      lclVarName
                      (LocalVarParam $ p <> NE.singleton lclVarInfo)
                      scope.lclVarInfo
                }
            _ ->
              let newLclVarInfo = Map.insert lclVarName (LocalVarParam $ NE.singleton lclVarInfo) scope.lclVarInfo
               in scope {lclVarInfo = newLclVarInfo}
    _ -> scope
