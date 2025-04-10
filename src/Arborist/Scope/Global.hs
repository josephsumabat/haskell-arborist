module Arborist.Scope.Global where

import AST
import Arborist.ProgramIndex
import Arborist.Scope.Types
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Debug.Trace
import Hir
import Hir.Types (Decl, ModuleText)
import Hir.Types qualified as Hir

data ExportedName = ExportedName
  { name :: T.Text
  , decl :: Decl
  , dynNode :: DynNode
  , mod :: ModuleText
  }
  deriving (Show)

type ExportIndex = Map.HashMap ModuleText [ExportedName]

declToExportedName :: ModuleText -> Decl -> ExportedName
declToExportedName modName decl =
  ExportedName
    { name = (declName decl).node.nodeText
    , dynNode = declDynNode decl
    , mod = modName
    , decl = decl
    }

exportToInfo :: ModuleText -> Bool -> ExportedName -> GlblNameInfo
exportToInfo importedFrom qualified expName =
  GlblNameInfo
    { name = expName.name
    , decl = expName.decl
    , dynNode = expName.dynNode
    , originatingMod = expName.mod
    , importedFrom = importedFrom
    , requiresQualifier = qualified
    }

infoToExport :: GlblNameInfo -> ExportedName
infoToExport glblInfo =
  ExportedName
    { name = glblInfo.name
    , decl = glblInfo.decl
    , dynNode = glblInfo.dynNode
    , mod = glblInfo.originatingMod
    }

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

-- | Get names that an import brings into the global scope
-- Respects qualification, hiding, import list etc.
getImportNames :: ProgramIndex -> ExportIndex -> Hir.Import -> ([GlblNameInfo], ExportIndex)
getImportNames prgIndex exportIndex thisImport =
  let qualified = thisImport.qualified
      mod = thisImport.mod
      (exportedNames, updatedExportIndex) = getExportedNames prgIndex exportIndex mod
      glblNameInfo = exportToInfo thisImport.mod qualified <$> exportedNames
      importNames = Set.fromList $ (.nodeText) . (.node) . (.name) <$> thisImport.importList
      importList
        -- return everything if no import list
        | null importNames = glblNameInfo
        -- Remove when hidden import list
        | thisImport.hiding = filter (\n -> not (Set.member n.name importNames)) glblNameInfo
        -- include only things in importlist
        | otherwise = filter (\n -> Set.member n.name importNames) glblNameInfo
   in (importList, updatedExportIndex)

-- | Get multiple import names caching re-exports along the way
getManyImportNames :: ProgramIndex -> ExportIndex -> [Hir.Import] -> ([GlblNameInfo], ExportIndex)
getManyImportNames prgIndex exportIndex imports =
  List.foldl'
    ( \(importedNamesAgg, idx) prg ->
        let (importedNames, nextIdx) = getImportNames prgIndex idx prg
         in (importedNames <> importedNamesAgg, nextIdx)
    )
    ([], exportIndex)
    imports

getTransitiveReExports :: Hir.Program -> [Hir.ExportItem] -> Set.Set T.Text
getTransitiveReExports prg exports =
  let declaredNames = fmap (\decl -> (declNameText decl)) prg.decls
      declaredNamesSet = Set.fromList declaredNames
      exportNamesSet =
        Set.fromList $
          (.node.nodeText)
            <$> (exportItemNames exports)
      transitiveReexportNames = exportNamesSet `Set.difference` declaredNamesSet
   in transitiveReexportNames

-- | get information about all names exported by a module
getExportedNames ::
  ProgramIndex ->
  ExportIndex ->
  ModuleText ->
  ([ExportedName], ExportIndex)
getExportedNames prgIndex exportIndex modName =
  let found = Map.lookup modName exportIndex
   in case found of
        Just v -> (v, exportIndex)
        Nothing ->
          case Map.lookup modName prgIndex of
            Nothing -> ([], exportIndex)
            Just prg -> getExportedNamesFromPrg prg
 where
  getExportedNamesFromPrg :: Hir.Program -> ([ExportedName], ExportIndex)
  getExportedNamesFromPrg prg =
    let declaredNames = getDeclaredNames modName prg
     in case prg.exports of
          -- If there is no export list then just declared names are exported
          Nothing ->
            let idxWithSelf = Map.insert modName declaredNames exportIndex
             in (declaredNames, idxWithSelf)
          Just exportLst ->
            let
              exportNamesSet =
                Set.fromList $
                  (.node.nodeText)
                    <$> (exportItemNames exportLst)
              transitiveReexportNames = getTransitiveReExports prg exportLst
              reExportedMods = Set.fromList $ (.mod) <$> exportItemMods exportLst
              -- Handle transitively re-exported names - avoid getting
              -- transitive dependencies that are not required
              requiredImports =
                if null transitiveReexportNames
                  then filter (\imp -> imp.mod `Set.member` reExportedMods) prg.imports
                  else prg.imports

              -- Use the export index with declared names in case of cycles
              idxWithEmpty = Map.insert modName [] exportIndex
              (allImportedNames, updatedExportIdx) =
                getManyImportNames prgIndex idxWithEmpty requiredImports
              transitiveReexportNamesInfo =
                filter
                  ( \expInfo ->
                      expInfo.name `Set.member` exportNamesSet
                        || expInfo.importedFrom `Set.member` reExportedMods
                  )
                  allImportedNames
              exportedNames =
                (infoToExport <$> transitiveReexportNamesInfo)
                  <> filter
                    (\expInfo -> expInfo.name `Set.member` exportNamesSet)
                    declaredNames
              updateExportIdxWithSelf =
                Map.insert modName exportedNames updatedExportIdx
             in
              (exportedNames, updateExportIdxWithSelf)

getDeclaredNames :: ModuleText -> Hir.Program -> [ExportedName]
getDeclaredNames mod prg =
  fmap (declToExportedName mod) prg.decls

getGlobalAvailableNames :: ProgramIndex -> ExportIndex -> Hir.Program -> [GlblNameInfo]
getGlobalAvailableNames availPrgs exportIdx thisPrg =
  let declaredNames =
        maybe
          []
          ( \modName ->
              exportToInfo modName False <$> getDeclaredNames modName thisPrg
          )
          thisPrg.mod
      (importedNames, _) = getManyImportNames availPrgs exportIdx thisPrg.imports
   in declaredNames <> importedNames

availableNamesToScope :: [GlblNameInfo] -> Scope
availableNamesToScope availNames = List.foldl' indexNameInfo emptyScope availNames
 where
  indexNameInfo :: Scope -> GlblNameInfo -> Scope
  indexNameInfo scope availName =
    let nameKey = availName.name
        originatingMod = availName.originatingMod
        importedMod = availName.importedFrom
        moduleKey = importedMod
        currentMap = scope.glblVarInfo
        modMap = Map.findWithDefault Map.empty nameKey currentMap
        existing = Map.findWithDefault [] moduleKey modMap

        (newEntry, rest) = case availName.decl of
          Hir.DeclBind b -> tryMergeBind b importedMod originatingMod existing
          Hir.DeclSig s -> tryMergeSig s importedMod originatingMod existing
          _ -> (Nothing, existing)

        updatedModMap = Map.insert importedMod (maybeToList newEntry ++ rest) modMap
        updatedVarMap = Map.insert nameKey updatedModMap currentMap
     in scope {glblVarInfo = updatedVarMap}

  -- If an existing bind or signature exists with the same fully qualified name add it to the same `glblVarInfo
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
