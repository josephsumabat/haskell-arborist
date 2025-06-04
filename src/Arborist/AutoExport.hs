module Arborist.AutoExport
  ( getAllDeclExportEdit,
    getDeclExportEdit
  ) where

import Hir.Types qualified as Hir
import AST.Haskell qualified as H
import AST qualified
import Data.Text (Text)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Text as Text
import Data.Edit as Edit ( Edit, empty )
import Data.Either.Extra (eitherToMaybe)
import Arborist.Rewrite (rewriteNode)

data ExportRewrite = ExportRewrite
  { name :: Text,
    renderedName :: Text 
  } deriving (Show, Eq)
    
-- convert all export decls to [ExportRewrite]  
getAllDeclNames :: Hir.Program -> [ExportRewrite]
getAllDeclNames prog = mapMaybe declToExportRewrite prog.decls

-- change to parseP and get 
declToExportRewrite :: Hir.Decl -> Maybe ExportRewrite
declToExportRewrite decl = 
  case decl of
    Hir.DeclBind bind -> 
      let name = bind.name
          nameText = name.node.nodeText
          wrapped
            | name.isOperator = "(" <> nameText <> ")"
            | otherwise = nameText
      in  Just (ExportRewrite nameText wrapped)
    Hir.DeclData decl  -> 
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ExportRewrite nameText renderedName)
    Hir.DeclNewtype decl  -> 
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ExportRewrite nameText renderedName)
    Hir.DeclClass decl  -> 
      let name = decl.name
          nameText = name.node.nodeText
          renderedName = nameText <> "(..)"
      in Just (ExportRewrite nameText renderedName)
    _ -> Nothing

-- update the export list, use if want to maintain old exports
getNewExportList :: [ExportRewrite] -> H.ExportsP -> Text
getNewExportList rewrites exportNode =
  let exportNodeDyn = (AST.getDynNode exportNode)
      names = map (\(ExportRewrite _ nm) -> nm) rewrites
      namesText = Text.intercalate ", " names
      originalExportList = exportNodeDyn.nodeText
      prefix
          | originalExportList == "()" = "("
          | otherwise = Text.init originalExportList <> ", "
  in
    prefix <> namesText <> ")"

-- create a completely new export list
createNewExportList :: [ExportRewrite] -> Text
createNewExportList rewrites =
  let
    names = map (\(ExportRewrite _ nm) -> nm) rewrites
    namesText = Text.intercalate "\n  , " names
  in
    "\n  ( " <> namesText <> "\n  )\n"

-- wrapper specific to exporting for rewriteNode
editExportList :: H.ExportsP -> [ExportRewrite] -> Edit
editExportList exportNode newExportRewriteList = rewriteNode (AST.getDynNode exportNode) (getNewExportList newExportRewriteList exportNode)

rewriteExportList :: H.ExportsP -> [ExportRewrite] -> Edit
rewriteExportList exportNode newExportRewriteList = rewriteNode (AST.getDynNode exportNode) (createNewExportList newExportRewriteList)

writeNewExportList :: H.HeaderUP -> [ExportRewrite]  -> Edit
writeNewExportList headerNode newExportRewriteList =
  let header = AST.getDynNode headerNode
      newExports = createNewExportList newExportRewriteList
      moduleNameTxt = (AST.getDynNode headerNode.module').nodeText
      newText = "module " <> moduleNameTxt <> newExports <> "where"
  in rewriteNode header newText

-- given the program, finds the export node and the new list of exports to create an Edit
getAllDeclExportEdit :: Hir.Program -> H.HeaderP -> Edit
getAllDeclExportEdit prog header =
  let allDecls = getAllDeclNames prog
      mHeaderU = eitherToMaybe $ AST.unwrap header
      mExports = mHeaderU >>= (.exports) 
  in
      case mExports of 
        Just export -> rewriteExportList export allDecls
        Nothing -> maybe Edit.empty (\header -> (writeNewExportList header allDecls)) mHeaderU

getDeclExportEdit :: H.HeaderP  -> Hir.Decl  -> Edit
getDeclExportEdit header newExport =
  let newDecl = (maybeToList (declToExportRewrite newExport))
      mHeaderU = eitherToMaybe $ AST.unwrap header
      mExports = mHeaderU >>= (.exports) 
  in
      case mExports of 
        Just export -> editExportList export newDecl
        Nothing -> maybe Edit.empty (\header -> (writeNewExportList header newDecl)) mHeaderU