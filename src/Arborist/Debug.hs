module Arborist.Debug where

import AST.Extension
import AST.Haskell
import qualified AST
import Arborist.Renamer

type PrintExt = Name RenamePhase AST.:+ Name ParsePhase AST.:+ Variable RenamePhase AST.:+ AST.Nil

-- Helper function to format a single node
debugNodeStr :: Int -> AST.DynNode -> Prelude.String
debugNodeStr indentLevel node =
  let indent = replicate (indentLevel * 2) ' ' -- Indentation for readability
      nodeTypeStr = indent <> "Node Type: " <> show (AST.nodeType node) <> "\n"
      separator = indent <> "--------------------------------------\n"
      printNode node =
            indent <> "Node Ext Type: " <> show (node.ext) <> "\n" <>
            indent <> "Node Text: " <> show (node.dynNode.nodeText) <> "\n"
      nodeStr =
        case AST.cast @PrintExt node of
          Just (AST.Inj @(Name RenamePhase) node) ->
            printNode node
          Just (AST.Inj @(Name ParsePhase) node) ->
            printNode node
          Just (AST.Inj @(Variable RenamePhase) node) ->
            printNode node
          Just _ -> ""
          Nothing -> ""
  in nodeTypeStr <> nodeStr <> separator

-- Recursive function to generate the entire tree's string representation
debugTreeStr :: Int -> AST.DynNode -> Prelude.String
debugTreeStr indentLevel node =
  let nodeStr = debugNodeStr indentLevel node
      childrenStr = concatMap (debugTreeStr (indentLevel + 1)) (AST.nodeChildren node)
  in nodeStr <> childrenStr

-- Entry point function with default indentation
debugTree :: AST.DynNode -> Prelude.String
debugTree = debugTreeStr 0
