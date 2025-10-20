{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Arborist.GlobImports (
  globImportModules,
  hasGlobImportComment,
) where

import AST qualified
import AST.Haskell qualified as AST
import Arborist.Files (ModFileMap)
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir

globImportModules :: ModFileMap -> Hir.ModuleText -> Hir.Read.Program -> [Hir.ModuleText]
globImportModules modFileMap moduleText program
  | isGlobModule moduleText && hasGlobImportComment program = siblingModules
  | otherwise =
      []
 where
  siblingModules =
    [ siblingMod
    | (siblingMod, _) <- Map.toList modFileMap
    , siblingMod /= moduleText
    , sharesPrefix siblingMod
    ]

  sharesPrefix siblingMod =
    let siblingParts = NE.toList siblingMod.parts
     in prefixParts `List.isPrefixOf` siblingParts && siblingParts /= prefixParts

  prefixParts = List.init (NE.toList moduleText.parts)

hasGlobImportComment :: Hir.Read.Program -> Bool
hasGlobImportComment program = any matches program.node.dynNode.nodeChildren
 where
  marker = Text.pack "-- GLOB_IMPORTS_SPLICE"

  matches dynNode =
    matchesMarker dynNode || any matchesMarker dynNode.nodeChildren

  matchesMarker dynNode =
    case AST.cast @AST.CommentP dynNode of
      Just comment ->
        any ((== marker) . Text.strip) (Text.lines comment.dynNode.nodeText)
      Nothing -> False

isGlobModule :: Hir.ModuleText -> Bool
isGlobModule moduleText =
  case NE.last moduleText.parts of
    namePart -> namePart == Text.pack "All"
