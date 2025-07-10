module Arborist.AutoImport where

import Hir.Types qualified as Hir
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

addToImportList :: ImportRewrite -> Hir.Import -> Text -> Text
addToImportList rewrite hirImport originalText =
  case hirImport.importList of
    Nothing -> originalText <> " (" <> rewrite.renderedName <> ")"
    Just [] ->
      let beforeParen = Text.takeWhile (/= '(') originalText
      in beforeParen <> "(" <> rewrite.renderedName <> ")"
    Just _ ->
      let beforeParen = Text.takeWhile (/= '(') originalText
          afterParen = Text.dropWhile (/= '(') originalText
          currentList = Text.drop 1 $ Text.dropEnd 1 afterParen
      in beforeParen <> "(" <> currentList <> ", " <> rewrite.renderedName <> ")"

addDeclToImportEdit :: AST.DynNode -> Hir.Import -> Hir.Decl -> Edit
addDeclToImportEdit importNode hirImport decl =
  case declToImportRewrite decl of
    Nothing -> Edit.empty
    Just importRewrite -> 
      let originalText = importNode.nodeText
          newText = addToImportList importRewrite hirImport originalText
      in rewriteNode importNode newText
