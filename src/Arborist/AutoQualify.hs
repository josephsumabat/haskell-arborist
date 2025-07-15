module Arborist.AutoQualify where

import Hir.Types qualified as Hir
import AST qualified
import Data.Edit as Edit (Edit, empty)
import Arborist.Rewrite (rewriteNode)

-- create an edit to qualify a usage with the appropriate qualifier
qualifyUsageEdit :: AST.DynNode -> Hir.Import -> Edit
qualifyUsageEdit usageNode hirImport =
  if hirImport.qualified
    then
      let originalName = usageNode.nodeText
          qualifier = case hirImport.alias of
            Just aliasModule -> aliasModule.text
            Nothing -> hirImport.mod.text
          qualifiedName = qualifier <> "." <> originalName
      in rewriteNode usageNode qualifiedName
    else Edit.empty

-- create an edit to qualify a declaration usage given its import
qualifyDeclUsage :: AST.DynNode -> Hir.Import -> Edit
qualifyDeclUsage declNode hirImport = qualifyUsageEdit declNode hirImport
