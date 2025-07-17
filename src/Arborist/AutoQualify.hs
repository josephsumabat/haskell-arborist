module Arborist.AutoQualify where

import Hir.Types qualified as Hir
import AST qualified
import Data.Edit as Edit (Edit, empty)
import Arborist.Rewrite (rewriteNode)

-- create an edit to qualify a usage with the appropriate qualifier
qualifyIdentifierEdit :: Hir.Name -> Hir.Import -> Edit
qualifyIdentifierEdit identifierName hirImport =
  if hirImport.qualified
    then
      let identifierNode = identifierName.node
          originalName = identifierNode.nodeText
          qualifier = case hirImport.alias of
            Just aliasModule -> aliasModule.text
            Nothing -> hirImport.mod.text
          qualifiedName = qualifier <> "." <> originalName
      in rewriteNode identifierNode qualifiedName
    else Edit.empty

-- create an edit to qualify a declaration usage given its import
qualifyIdentifier :: Hir.Name -> Hir.Import -> Edit
qualifyIdentifier identifierName hirImport = qualifyIdentifierEdit identifierName hirImport
