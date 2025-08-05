module Arborist.AutoQualify where

import AST qualified
import Arborist.Rewrite (rewriteNode)
import Data.Edit as Edit (Edit, empty)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir

-- create an edit to qualify a usage with the appropriate qualifier
qualifyIdentifierEdit :: Hir.Read.Name -> Hir.Read.Import -> Edit
qualifyIdentifierEdit identifierName hirImport =
  if hirImport.qualified
    then
      let identifierNode = identifierName.dynNode
          originalName = identifierNode.nodeText
          qualifier = case hirImport.alias of
            Just aliasModule -> aliasModule.text
            Nothing -> hirImport.mod.text
          qualifiedName = qualifier <> "." <> originalName
       in rewriteNode identifierNode qualifiedName
    else Edit.empty

-- create an edit to qualify a declaration usage given its import
qualifyIdentifier :: Hir.Read.Name -> Hir.Read.Import -> Edit
qualifyIdentifier identifierName hirImport = qualifyIdentifierEdit identifierName hirImport
