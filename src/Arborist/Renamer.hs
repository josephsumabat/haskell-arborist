{-# LANGUAGE TypeFamilies #-}
module Arborist.Renamer where
import AST.Extension
import AST.Haskell
import qualified AST
import AST.Haskell.Generated qualified as AST
import qualified Data.Map.Lazy as Map
import Data.LineColRange
import qualified Data.Text as T

data RenamePhase

data ResolvedName = ResolvedName


-- type instance XName RenamePhase = ResolvedName
instance NodeX RenamePhase where
  type XName RenamePhase = ResolvedName

type HaskellR = Haskell RenamePhase

data ScopeData =
  ScopeData
    {
      bindingName :: T.Text
    , loc :: LineColRange
    }

type Rib = Map.Map T.Text ScopeData

renameHaskell :: Haskell ParsePhase -> Maybe (Haskell RenamePhase)
renameHaskell haskell = AST.cast @HaskellR (go haskell.dynNode)
    where
      go :: AST.DynNode -> AST.DynNode
      go n = (rename n) { AST.nodeChildren = rename <$> n.nodeChildren }
      
      rename :: AST.DynNode -> AST.DynNode
      rename n =
        case AST.cast @NameP n of
          Nothing -> n
          Just nameNode ->
            AST.getDynNode $ AST.modifyNameExt @RenamePhase nameNode (\_ -> ResolvedName)


buildScope :: AST.DynNode -> ScopeData
buildScope = undefined
