module Arborist.Exports (
  getTransitiveReExports,
  modsFromAliases,
  isExportedImport,
  getAliasModMap,
  AliasModMap,
)
where

import AST
import Control.Error
import Control.Monad
import Data.HashMap.Strict qualified as Map
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as T
import Hir
import Hir.Types (ModuleText)
import Hir.Types qualified as Hir

-- | Will include module name -> itself it the module is not imported with an alias
type AliasModMap = Map.HashMap ModuleText [ModuleText]

-- | Build a map of all alias -> mod names that the alias may refer to
-- Will include module name -> itself it the module is not imported with an alias
getAliasModMap :: [Hir.Import] -> Map.HashMap ModuleText [ModuleText]
getAliasModMap imports =
  List.foldl'
    ( \acc imp ->
        maybe
          (Map.insertWith (++) imp.mod [imp.mod] acc)
          (\alias -> Map.insertWith (++) alias [imp.mod] acc)
          imp.alias
    )
    Map.empty
    imports

getTransitiveReExports :: Hir.Program -> [Hir.ExportItem] -> Set.Set T.Text
getTransitiveReExports prg exports =
  let declaredNames = fmap declNameText prg.decls
      declaredNamesSet = Set.fromList declaredNames
      exportNamesSet = Set.fromList $ (.node.nodeText) <$> exportItemNames exports
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
