module Arborist.Debug (
  traceShowPretty,
  debugTree,
)
where

import AST qualified
import AST.Extension
import AST.Haskell
import Arborist.Renamer
import Data.Text.Lazy qualified as Text
import Debug.Trace
import Text.Pretty.Simple

type PrintExt = Module RenamePhase AST.:+ Name RenamePhase AST.:+ Name ParsePhase AST.:+ Variable RenamePhase AST.:+ AST.Nil

traceShowPretty :: (Show s) => s -> a -> a
traceShowPretty p v = trace (Text.unpack . pShowNoColor $ p) v

-- Helper function to format a single node
debugNodeStr :: Int -> AST.DynNode -> Prelude.String
debugNodeStr indentLevel node =
  let indent = replicate (indentLevel * 2) ' ' -- Indentation for readability
      nodeTypeStr = indent <> "Node Type: " <> show (AST.nodeType node) <> "\n"
      separator = indent <> "--------------------------------------\n"
      printDynNode dynNode =
        indent
          <> "Node Loc: "
          <> show (dynNode.nodeLineColRange)
          <> "\n"
          <> indent
      printNoExt node =
        printDynNode node.dynNode
          <> "Node Text: "
          <> show (node.dynNode.nodeText)
          <> "\n"
      printNode node =
        indent
          <> "Node Ext Type: "
          <> "\n"
          <> addIndentation (indentLevel * 2) ((Text.unpack . pShowNoColor) (node.ext))
          <> "\n"
          <> indent
          <> "Node Text: "
          <> show (node.dynNode.nodeText)
          <> "\n"
          <> indent
          <> "Node Loc: "
          <> show (node.dynNode.nodeLineColRange)
          <> "\n"
      nodeStr =
        case AST.cast @PrintExt node of
          Just (AST.Inj @(Module RenamePhase) node) ->
            printNoExt node
          Just (AST.Inj @(Name RenamePhase) node) ->
            printNode node
          Just (AST.Inj @(Name ParsePhase) node) ->
            printNode node
          Just (AST.Inj @(Variable RenamePhase) node) ->
            printNode node
          Just _ -> printDynNode node
          Nothing -> printDynNode node
   in nodeTypeStr <> nodeStr <> separator

addIndentation :: Int -> Prelude.String -> Prelude.String
addIndentation n = unlines . map (replicate (n * 2) ' ' ++) . lines

-- Recursive function to generate the entire tree's string representation
debugTreeStr :: Int -> AST.DynNode -> Prelude.String
debugTreeStr indentLevel node =
  let nodeStr = debugNodeStr indentLevel node
      childrenStr = concatMap (debugTreeStr (indentLevel + 1)) (AST.nodeChildren node)
   in nodeStr <> childrenStr

-- Entry point function with default indentation
debugTree :: AST.DynNode -> Prelude.String
debugTree = debugTreeStr 0
