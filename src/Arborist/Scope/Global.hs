module Arborist.Scope.Global where

import Arborist.ModGraph
import AST
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Maybe
import Hir
import Hir.Types (Decl, ModuleText)
import Hir.Types qualified as Hir
import Arborist.Scope.Types


declToNameInfo :: ModuleText -> ModuleText -> Bool -> Decl -> GlblNameInfo
declToNameInfo originatingMod importedFrom qualified decl =
  GlblNameInfo
    { name = (declName decl).node.nodeText
    , dynNode = declDynNode decl
    , originatingMod = originatingMod
    , importedFrom = importedFrom
    , requiresQualifier = qualified
    , decl = decl
    }

getNamesFromImport :: ProgramIndex -> Hir.Import -> [GlblNameInfo]
getNamesFromImport availPrgs thisImport =
  let qualified = thisImport.qualified
      foundImport = Map.lookup thisImport.mod availPrgs
   in case foundImport of
        Nothing -> []
        Just i -> 
          getImportExportedNames availPrgs i qualified

getImportExportedNames :: ProgramIndex -> Hir.Program -> Bool -> [GlblNameInfo]
getImportExportedNames availPrgs prg qualified =
  let declaredInImport modName = fmap (declToNameInfo modName modName qualified) prg.decls
      reExported = []
   in case prg.mod of
        Nothing -> []
        Just modName -> declaredInImport modName <> reExported

getGlobalAvailableNames :: ProgramIndex -> Hir.Program -> [GlblNameInfo]
getGlobalAvailableNames availPrgs thisPrg =
  let declaredNames =
        case thisPrg.mod of
          Nothing -> []
          Just m -> fmap (declToNameInfo m m False) thisPrg.decls
      importedNames = getNamesFromImport availPrgs =<< thisPrg.imports
   in declaredNames <> importedNames

availableNamesToScope :: [GlblNameInfo] -> Scope
availableNamesToScope availNames = List.foldl' indexNameInfo emptyScope availNames
 where
  indexNameInfo :: Scope -> GlblNameInfo -> Scope
  indexNameInfo scope availName =
    let nameKey = availName.name
        moduleKey = availName.originatingMod
        importedMod = moduleKey -- TODO - handle
        currentMap = scope.glblVarInfo
        modMap = Map.findWithDefault Map.empty nameKey currentMap
        existing = Map.findWithDefault [] moduleKey modMap

        (newEntry, rest) = case availName.decl of
          Hir.DeclBind b -> tryMergeBind b importedMod moduleKey existing
          Hir.DeclSig s -> tryMergeSig s importedMod moduleKey existing
          _ -> (Nothing, existing)

        updatedModMap = Map.insert moduleKey (maybeToList newEntry ++ rest) modMap
        updatedVarMap = Map.insert nameKey updatedModMap currentMap
     in scope {glblVarInfo = updatedVarMap}

  tryMergeBind :: Hir.BindDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeBind b importedFrom origMod [] =
    (Just (GlblVarInfo {sig = Nothing, binds = [b], importedFrom, originatingMod = origMod, name = b.name}), [])
  tryMergeBind b importedFrom origMod (v : vs)
    | null v.binds =
        let merged = v {binds = [b], importedFrom}
         in (Just merged, vs)
    | otherwise =
        let (result, rest) = tryMergeBind b importedFrom origMod vs
         in (result, v : rest)

  tryMergeSig :: Hir.SigDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeSig s importedFrom origMod [] =
    (Just (GlblVarInfo {sig = Just s, binds = [], importedFrom, originatingMod = origMod, name = s.name}), [])
  tryMergeSig s importedFrom origMod (v : vs)
    | isNothing v.sig =
        let merged = v {sig = Just s, importedFrom}
         in (Just merged, vs)
    | otherwise =
        let (result, rest) = tryMergeSig s importedFrom origMod vs
         in (result, v : rest)
