module Arborist.Haddock (
  indexPrgHaddocks,
  indexManyPrgHaddocks,
  HaddockInfo (..),
  QualifiedName (..),
)
where

import AST qualified
import AST.Haskell qualified as AST
import Data.Either.Extra
import Data.HashMap.Lazy qualified as Map
import Data.Hashable
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as T
import Hir qualified
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import Safe

data HaddockInfo = HaddockInfo
  { text :: T.Text
  }
  deriving (Show, Eq)

data QualifiedName = QualifiedName
  { mod :: Hir.ModuleText
  , name :: T.Text
  }
  deriving (Show, Eq)

instance Hashable QualifiedName where
  hashWithSalt s n = hashWithSalt s (n.mod.text, n.name)

type HaddockIndex = Map.HashMap QualifiedName HaddockInfo

-- | Index haddocks for multiple programs
indexManyPrgHaddocks :: HaddockIndex -> [Hir.Program Hir.HirRead] -> HaddockIndex
indexManyPrgHaddocks haddockIndex prgs =
  List.foldl' (\acc prg -> indexPrgHaddocks acc prg) haddockIndex prgs

indexPrgHaddocks :: HaddockIndex -> Hir.Program Hir.HirRead -> HaddockIndex
indexPrgHaddocks haddockIndex prg =
  case prg.mod of
    Nothing ->
      haddockIndex
    Just mod ->
      let mHaskellU = eitherToMaybe $ AST.unwrap prg.node
          mImportsU = mHaskellU >>= (.imports) >>= eitherToMaybe . AST.unwrap
          mDeclarations = mHaskellU >>= (.declarations)
          mDeclarationsU = mDeclarations >>= eitherToMaybe . AST.unwrap
          initialHaddock =
            fromMaybe Map.empty $
              (tryInitialHaddock haddockIndex mod)
                <$> mImportsU
                <*> mDeclarationsU
          declHaddocks =
            maybe
              haddockIndex
              (\decls -> step initialHaddock mod decls.dynNode.nodeChildren)
              mDeclarationsU
       in declHaddocks

-- | Special case of initial haddock - the haddock is parsed as a part of imports instead of declarations
-- so we attempt to check to see if the last import child is a haddock and the first declaration has a name
-- we can attach it to
tryInitialHaddock :: HaddockIndex -> Hir.ModuleText -> AST.ImportsUP -> AST.DeclarationsUP -> HaddockIndex
tryInitialHaddock haddockIndex mod imps decls =
  let mLastImport = lastMay imps.dynNode.nodeChildren
      mFstDecl = headMay decls.dynNode.nodeChildren
   in case ( AST.cast @AST.HaddockP =<< mLastImport
           , AST.cast @AST.DeclarationP =<< mFstDecl
           ) of
        (Just haddockNode, Just declNode) ->
          let mName = eitherToMaybe $ Hir.parseDeclaration declNode
           in case mName of
                Just (decl : _) ->
                  let name = Hir.declName decl
                      qualifiedName = QualifiedName mod (name.nameText)
                      doc = HaddockInfo {text = haddockNode.dynNode.nodeText}
                      nextHaddockIndex = Map.insert qualifiedName doc haddockIndex
                   in nextHaddockIndex
                _ -> haddockIndex
        _ -> haddockIndex

-- | General case, look ahead trying to find haddocks and names to attach them to
step :: HaddockIndex -> Hir.ModuleText -> [AST.DynNode] -> HaddockIndex
step !haddockIndex _mod [] = haddockIndex
step !haddockIndex _mod [_nodeChild] = haddockIndex
step !haddockIndex mod (potentialHaddock : potentialDecl : rest) =
  case ( AST.cast @AST.HaddockP potentialHaddock
       , AST.cast @AST.DeclarationP potentialDecl
       ) of
    (Just haddockNode, Just declNode) ->
      let mName = eitherToMaybe $ Hir.parseDeclaration declNode
       in case mName of
            Just (decl : _) ->
              let name = Hir.declName decl
                  qualifiedName = QualifiedName mod (name.nameText)
                  doc = HaddockInfo {text = haddockNode.dynNode.nodeText}
                  nextHaddockIndex = Map.insert qualifiedName doc haddockIndex
               in step nextHaddockIndex mod rest
            Nothing -> step haddockIndex mod rest
            Just [] -> step haddockIndex mod rest
    -- skip if a decl as it wont be a haddock
    (Nothing, Just _decl) -> step haddockIndex mod rest
    -- potentialDecl could be a haddock in these cases
    (Just _, Nothing) -> step haddockIndex mod (potentialDecl : rest)
    (Nothing, Nothing) -> step haddockIndex mod (potentialDecl : rest)
