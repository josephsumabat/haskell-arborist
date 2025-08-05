{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Arborist.Exports (
  getTransitiveReExportNames,
  getNameExportInfo,
  modsFromAliases,
  isExportedImport,
  getAliasModMap,
  AliasModMap,
)
where

import AST
import Arborist.Scope.Types
import Control.Error
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Hir
import Hir.Read.Types qualified as Hir
import Hir.Types

-- | Will include module name -> itself it the module is not imported with an alias
type AliasModMap = Map.HashMap ModuleText [ModuleText]

-- | Build a map of all alias -> mod names that the alias may refer to
-- Will include module name -> itself it the module is not imported with an alias
getAliasModMap :: Hir.Program -> Map.HashMap ModuleText [ModuleText]
getAliasModMap prg =
  let imports = prg.imports
      withSelf = maybe Map.empty (\mod -> Map.insert mod [mod] Map.empty) prg.mod
   in List.foldl'
        ( \acc imp ->
            maybe
              (Map.insertWith (++) imp.mod [imp.mod] acc)
              (\alias -> Map.insertWith (++) alias [imp.mod] acc)
              imp.alias
        )
        withSelf
        imports

getNameExportInfo :: [GlblDeclInfo] -> [Hir.ExportItem] -> [GlblDeclInfo]
getNameExportInfo availNames exports =
  let exportSet = getSearchableExportSet exports
   in filter (\info -> any (`Set.member` exportSet) (lookupKeys info)) availNames
 where
  -- Try both unqualified and qualified keys if unqualified is allowed
  lookupKeys :: GlblDeclInfo -> [(T.Text, Maybe ModuleText)]
  lookupKeys info =
    let name = info.name
        qual = Just info.importedFrom.namespace
     in if info.requiresQualifier
          then [(name, qual)]
          else [(name, Nothing), (name, qual)]

  getSearchableExportSet :: [Hir.ExportItem] -> Set.Set (T.Text, Maybe ModuleText)
  getSearchableExportSet = Set.fromList . map toPair . exportItemNames
   where
    toPair qual = (qual.name.nameText, (.mod) <$> qual.mod)

-- | Get names that are not declared in the current program
getTransitiveReExportNames :: Hir.Program -> [Hir.ExportItem] -> Set.Set T.Text
getTransitiveReExportNames prg exports =
  let declaredNames = fmap declNameText prg.decls
      declaredNamesSet = Set.fromList declaredNames
      exportNamesSet = Set.fromList $ (.name.nameText) <$> exportItemNames exports
   in exportNamesSet `Set.difference` declaredNamesSet

modsFromAliases :: Map.HashMap ModuleText [ModuleText] -> [ModuleText] -> Set.Set ModuleText
modsFromAliases aliasModMap aliasOrModNames =
  Set.fromList $
    join $
      mapMaybe (\alias -> Map.lookup alias aliasModMap) aliasOrModNames

-- | Requires exportedModNames to be module names NOT aliases
isExportedImport :: Set.Set ModuleText -> Hir.Import -> Bool
isExportedImport exportedModNames imp =
  imp.mod `Set.member` exportedModNames
