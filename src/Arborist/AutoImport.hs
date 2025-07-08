module Arborist.AutoImport where

import Hir.Types qualified as Hir
import AST.Haskell qualified as H
import AST qualified
import Data.Edit as Edit ( Edit, empty )
import Arborist.Rewrite (rewriteNode)
import Data.Text as Text

data ImportRewrite = ImportRewrite
  { name :: Text,
    renderedName :: Text
  } deriving (Show, Eq)

declToImportRewrite :: Hir.Decl -> Maybe ImportRewrite
declToImportRewrite decl =
  case decl of
    Hir.DeclBind bind ->
      let name = bind.name
          nameText = name.node.nodeText
          wrapped
            | name.isOperator = "(" <> nameText <> ")"
            | otherwise = nameText
      in  Just (ImportRewrite nameText wrapped)
    Hir.DeclSig sig ->
      let name = sig.name
          nameText = name.node.nodeText
          wrapped
            | name.isOperator = "(" <> nameText <> ")"
            | otherwise = nameText
      in  Just (ImportRewrite nameText wrapped)
    Hir.DeclData decl  ->
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ImportRewrite nameText renderedName)
    Hir.DeclNewtype decl  ->
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ImportRewrite nameText renderedName)
    Hir.DeclClass decl  ->
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ImportRewrite nameText renderedName)
    _ -> Nothing

-- add to existing import list (need to add special cases, no (), alias, qualified, etc.)
addToImportList :: ImportRewrite -> H.ImportP -> Text
addToImportList rewrite importNode =
  let importNodeDyn = AST.getDynNode importNode
      newItem = rewrite.renderedName
      originalImportText = importNodeDyn.nodeText
      beforeParen = Text.takeWhile (/= '(') originalImportText
      afterParen = Text.dropWhile (/= '(') originalImportText
      currentList = Text.drop 1 $ Text.dropEnd 1 afterParen
      newList = if Text.null (Text.strip currentList)
                then newItem
                else currentList <> ", " <> newItem
  in beforeParen <> "(" <> newList <> ")"


addDeclToImportEdit :: H.ImportP -> Hir.Decl -> Edit
addDeclToImportEdit importNode decl =
  case declToImportRewrite decl of
    Nothing -> Edit.empty
    Just importRewrite -> rewriteNode (AST.getDynNode importNode) (addToImportList importRewrite importNode)


