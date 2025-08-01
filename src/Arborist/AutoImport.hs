module Arborist.AutoImport where

import AST qualified
import Arborist.Rewrite (rewriteNode)
import Data.Edit as Edit (Edit, empty)
import Data.Text as Text
import Hir qualified
import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir

declToRenderItem :: Hir.Decl -> Maybe Render.ImportItem
declToRenderItem decl =
  let name = Hir.declName decl
      nameText = name.node.nodeText
      renderName =
        Render.Name
          { nameText = nameText
          , isOperator = name.isOperator
          , isConstructor = name.isConstructor
          }
      children =
        case decl of
          Hir.DeclBind _bind -> []
          Hir.DeclSig _sig -> []
          Hir.DeclData _decl -> [Render.ImportAllChildren]
          Hir.DeclNewtype _decl -> [Render.ImportAllChildren]
          Hir.DeclClass _decl -> [Render.ImportAllChildren]
          _ -> []
   in Just
        ( Render.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = renderName
            , children
            }
        )

addDeclToImportEdit :: Hir.Import -> Hir.Decl -> Edit
addDeclToImportEdit hirImport decl =
  case declToRenderItem decl of
    Nothing -> Edit.empty
    Just newImportItem ->
      let newText = Render.renderImport $ Render.addToImportList (Render.fromHirImport hirImport) newImportItem
       in rewriteNode hirImport.dynNode newText
