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
import Debug.Trace
import Hir
import Hir.Types (ModuleText)
import Hir.Types qualified as Hir

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

-- | Get all name infos that are explicitly exported by name
getNameExportInfo :: AliasModMap -> [GlblDeclInfo] -> [Hir.ExportItem] -> [GlblDeclInfo]
getNameExportInfo aliasModMap availNames exports =
  let exportSet = getSearchableExportSet exports
   in filter
        ( \info ->
            let imp = searchableInfo info
             in (info.name, imp) `Set.member` exportSet
        )
        availNames
 where
  -- If a name comes from this module then it will be handled as `Nothing` in the export set
  searchableInfo info =
    if info.importedFrom == info.originatingMod
      then Nothing
      else Just info.importedFrom

  -- A special searchable export set that handles qualifiers and module self-exports
  getSearchableExportSet exports =
    Set.fromList $
      ( \qual ->
          let potentialMods =
                case qual.mod of
                  Nothing -> [Nothing]
                  Just mod -> Just <$> fromMaybe [] (Map.lookup mod.mod aliasModMap)
           in (qual.name.node.nodeText,) <$> potentialMods
      )
        =<< exportItemNames exports

-- | Get names that are not declared in the current program
getTransitiveReExportNames :: Hir.Program -> [Hir.ExportItem] -> Set.Set T.Text
getTransitiveReExportNames prg exports =
  let declaredNames = fmap declNameText prg.decls
      declaredNamesSet = Set.fromList declaredNames
      exportNamesSet = Set.fromList $ (.name.node.nodeText) <$> exportItemNames exports
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
