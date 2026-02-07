module Arborist.AutoImport where

import AST qualified
import Arborist.Rewrite (rewriteNode)
import Data.Edit as Edit (Edit, empty)
import Data.Text as Text
import Hir qualified
import Hir.Read.Types qualified as Hir.Read
import Hir.Render.Import qualified as Render
import Hir.Types qualified as Hir
import Hir.Write.Types qualified as Hir.Write

declToRenderItem :: Hir.Read.Decl -> Maybe (Hir.Write.ImportItem)
declToRenderItem decl =
  let name = Hir.declName decl
      nameText = name.nameText
      renderName =
        Hir.Name
          { nameText = nameText
          , isOperator = name.isOperator
          , isConstructor = name.isConstructor
          , dynNode = ()
          }
      children =
        case decl of
          Hir.DeclBind _bind -> []
          Hir.DeclSig _sig -> []
          Hir.DeclData _decl -> [Hir.ImportAllChildren]
          Hir.DeclNewtype _decl -> [Hir.ImportAllChildren]
          Hir.DeclClass _decl -> [Hir.ImportAllChildren]
          _ -> []
   in Just
        ( Hir.ImportItem
            { namespace = Hir.NameSpaceValue
            , name = renderName
            , children
            }
        )

addDeclToImportEdit :: Hir.Read.Import -> Hir.Read.Decl -> Edit
addDeclToImportEdit hirImport decl =
  case declToRenderItem decl of
    Nothing -> Edit.empty
    Just newImportItem ->
      let newText = Render.renderImport $ Render.addToImportList (Render.fromReadImport hirImport) newImportItem
       in rewriteNode hirImport.dynNode newText
