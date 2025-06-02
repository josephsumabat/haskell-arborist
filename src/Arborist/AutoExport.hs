module Arborist.AutoExport
  ( getAllDeclExportEdits,
    getDeclExportEdits
  ) where

import Hir.Types qualified as Hir
import Hir (declNameText)
import AST (DynNode)
import AST.Haskell qualified as H
import AST qualified
import Hir.Parse
import Data.Text (Text)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Text as Text
import Data.Range (Range(..))
import Data.Edit (Edit, insert, empty)


data ExportRewrite = ExportRewrite
  { name :: Text
  } deriving (Show, Eq)
    
-- convert all export decls to [ExportRewrite]  
getAllDeclNames :: Hir.Program -> [ExportRewrite]
getAllDeclNames prog = mapMaybe declToExportRewrite prog.decls

declToExportRewrite :: Hir.Decl -> Maybe ExportRewrite
declToExportRewrite decl = 
  case decl of
    Hir.DeclBind _ -> Just (ExportRewrite (declNameText decl))
    _ -> Nothing

-- create new export list
getNewExportList :: Bool -> [ExportRewrite] -> Text -> Text
getNewExportList exportAllDecls rewrites originalExportList =
  let
    names = map (\(ExportRewrite nm) -> nm) rewrites
    namesText = Text.intercalate ", " names
  in
    if exportAllDecls 
      then "(" <> namesText <> ")"
    else 
        let prefix
              | originalExportList == "()" = "("
              | otherwise = Text.init originalExportList <> ", "
        in
          prefix <> namesText <> ")"

-- get the export node
locateExportNode :: H.HaskellP -> Maybe H.ExportsP
locateExportNode root = findNode (AST.cast @H.ExportsP) (AST.getDynNode root)


-- wrapper specific to exporting for rewriteNode
rewriteExport :: H.ExportsP -> [ExportRewrite] -> Bool -> Edit
rewriteExport exportNode newExportRewriteList exportAllDecls=
  let dynNode = AST.getDynNode exportNode
  in rewriteNode dynNode (getNewExportList exportAllDecls newExportRewriteList)


rewriteNode :: DynNode -> (Text -> Text) -> Edit
rewriteNode dynNode rewriteFunction =
  let oldText = dynNode.nodeText
      newText = rewriteFunction oldText
      Range start _ = dynNode.nodeRange
  in insert start newText

-- given the program, finds the export node and the new list of exports to create an Edit
getAllDeclExportEdits :: Hir.Program -> Edit
getAllDeclExportEdits prog =
  case locateExportNode prog.node of
    Nothing -> empty
    Just node ->
      let allDecls = getAllDeclNames prog
      in rewriteExport node allDecls True

getDeclExportEdits :: Hir.Program -> Hir.Decl  -> Edit
getDeclExportEdits prog newExport =
  case locateExportNode prog.node of
    Nothing -> empty
    Just node -> rewriteExport node (maybeToList (declToExportRewrite newExport)) False