module Arborist.Scope.Global (
  getExportedDecls,
  getImportDecls,
  globalNamesToScope,
  getGlobalAvailableNames,
  ExportIndex,
  declToExportedName,
  exportToInfo,
  infoToExport,
)
where

import AST
import Arborist.Exports
import Arborist.ProgramIndex
import Arborist.Scope.Types
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NES
import Data.Text qualified as T
import GHC.Stack
import Hir
import Hir.Types (Decl, ModuleText)
import Hir.Types qualified as Hir

data ExportedDecl = ExportedDecl
  { name :: T.Text
  , mod :: ModuleText
  , decl :: Decl
  }
  deriving (Show)

type ExportIndex = Map.HashMap ModuleText [ExportedDecl]

declToExportedName :: ModuleText -> Decl -> ExportedDecl
declToExportedName modName decl =
  ExportedDecl
    { name = (declName decl).node.nodeText
    , mod = modName
    , decl = decl
    }

exportToInfo :: ImportInfo -> Bool -> ExportedDecl -> GlblDeclInfo
exportToInfo importedFrom qualified expName =
  GlblDeclInfo
    { name = expName.name
    , decl = expName.decl
    , originatingMod = expName.mod
    , importedFrom = importedFrom
    , requiresQualifier = qualified
    }

infoToExport :: GlblDeclInfo -> ExportedDecl
infoToExport glblInfo =
  ExportedDecl
    { name = glblInfo.name
    , decl = glblInfo.decl
    , mod = glblInfo.originatingMod
    }

-- | Get all declarations exported by a given export.
-- Requires dependent programs in `ProgramIndex` or else will not find given imports
getExportedDecls :: (HasCallStack) => ProgramIndex -> ExportIndex -> ModuleText -> ([ExportedDecl], ExportIndex)
getExportedDecls prgIdx exportIdx modName =
  getExportedDecls' prgIdx exportIdx Set.empty modName

getManyImportDecls :: ProgramIndex -> ExportIndex -> [Hir.Import] -> ([GlblDeclInfo], ExportIndex)
getManyImportDecls prgIdx exportIdx imps =
  getManyImportDecls' prgIdx exportIdx Set.empty imps

-- | Get all declarations from a given import
-- Requires dependent programs in `ProgramIndex` or else will not find given imports
getImportDecls :: (HasCallStack) => ProgramIndex -> ExportIndex -> Hir.Import -> ([GlblDeclInfo], ExportIndex)
getImportDecls prgIdx exportIdx imp =
  getImportDecls' prgIdx exportIdx Set.empty imp

-- Internal cycle-safe versions
getImportDecls' ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  Hir.Import ->
  ([GlblDeclInfo], ExportIndex)
getImportDecls' prgIndex exportIndex inProgress thisImport =
  let qualified = thisImport.qualified
      mod = thisImport.mod
      alias = fromMaybe thisImport.mod thisImport.alias
      (exportedNames, updatedExportIndex) = getExportedDecls' prgIndex exportIndex inProgress mod
      importInfo =
        ImportInfo
          { mod = mod
          , namespace = alias
          }
      glblNameInfo = exportToInfo importInfo qualified <$> exportedNames
      importNames = Set.fromList $ (.nodeText) . (.node) . (.name) <$> thisImport.importList
      importList
        | null importNames = glblNameInfo
        | thisImport.hiding = filter (\n -> not (Set.member n.name importNames)) glblNameInfo
        | otherwise = filter (\n -> Set.member n.name importNames) glblNameInfo
   in (importList, updatedExportIndex)

getManyImportDecls' ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  [Hir.Import] ->
  ([GlblDeclInfo], ExportIndex)
getManyImportDecls' prgIndex exportIndex inProgress imports =
  List.foldl'
    ( \(importedNamesAgg, idx) imp ->
        let (importedNames, nextIdx) = getImportDecls' prgIndex idx inProgress imp
         in (importedNames <> importedNamesAgg, nextIdx)
    )
    ([], exportIndex)
    imports

getExportedDecls' ::
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  ModuleText ->
  ([ExportedDecl], ExportIndex)
getExportedDecls' prgIndex exportIndex inProgress modName
  | modName `Set.member` inProgress = ([], exportIndex)
  | Just exports <- Map.lookup modName exportIndex = (exports, exportIndex)
  | Just prg <- Map.lookup modName prgIndex = getExportedDeclsFromPrg prg
  | otherwise = ([], exportIndex)
 where
  getExportedDeclsFromPrg :: (HasCallStack) => Hir.Program -> ([ExportedDecl], ExportIndex)
  getExportedDeclsFromPrg prg =
    let declaredNames = getDeclaredNames modName prg
        inProgress' = Set.insert modName inProgress
     in case prg.exports of
          Nothing ->
            let exportIdxWithSelf = Map.insert modName declaredNames exportIndex
             in (declaredNames, exportIdxWithSelf)
          Just exportLst ->
            let
              transitiveReexportNames = getTransitiveReExportNames prg exportLst
              aliasModMap = getAliasModMap prg

              reExportedAliases = (.mod) <$> exportItemMods exportLst
              reExportedAliasesSet = Set.fromList reExportedAliases
              reExportedModsSet =
                modsFromAliases aliasModMap reExportedAliases
              requiredImports =
                if null transitiveReexportNames
                  then
                    filter
                      (isExportedImport reExportedModsSet)
                      prg.imports
                  else prg.imports

              (allImportedNames, updatedExportIdx) =
                getManyImportDecls' prgIndex exportIndex inProgress' requiredImports

              thisModImportInfo =
                ImportInfo
                  { mod = modName
                  , namespace = modName
                  }

              declaredNamesInfo = exportToInfo thisModImportInfo False <$> declaredNames
              allAvailableNames =
                declaredNamesInfo
                  <> allImportedNames

              moduleExports =
                filter
                  ( \expInfo ->
                      (expInfo.importedFrom.namespace `Set.member` reExportedAliasesSet)
                        && (not expInfo.requiresQualifier)
                  )
                  allAvailableNames

              nameExports =
                getNameExportInfo allAvailableNames exportLst

              allExportedNames =
                infoToExport <$> (nameExports <> moduleExports)

              updateExportIdxWithSelf = Map.insert modName allExportedNames updatedExportIdx
             in
              (allExportedNames, updateExportIdxWithSelf)

getDeclaredNames :: ModuleText -> Hir.Program -> [ExportedDecl]
getDeclaredNames mod prg =
  fmap (declToExportedName mod) prg.decls

getGlobalAvailableNames :: ProgramIndex -> ExportIndex -> Hir.Program -> [GlblDeclInfo]
getGlobalAvailableNames availPrgs exportIdx thisPrg =
  let
    declaredNames =
      maybe
        []
        ( \modName ->
            let thisModImportInfo = (ImportInfo modName modName)
             in exportToInfo thisModImportInfo False <$> getDeclaredNames modName thisPrg
        )
        thisPrg.mod
    (importedNames, _) = getManyImportDecls availPrgs exportIdx thisPrg.imports
   in
    declaredNames <> importedNames

-- | From a list of annotated declarations, attempt to build a scope - will try to
-- merge associated declarations together (e.g. a type signature and multiple binds)
globalNamesToScope :: [GlblDeclInfo] -> Scope
globalNamesToScope availNames = List.foldl' indexNameInfo emptyScope availNames
 where
  indexNameInfo :: Scope -> GlblDeclInfo -> Scope
  indexNameInfo scope availName =
    let nameKey = availName.name
        originatingMod = availName.originatingMod
        importedMod = availName.importedFrom
        moduleKey = importedMod
        currentMap = scope.glblVarInfo
        modMap = Map.findWithDefault Map.empty nameKey currentMap
        existing = Map.findWithDefault [] moduleKey modMap

        (newEntry, rest) = case availName.decl of
          Hir.DeclBind b -> tryMergeBind b availName.requiresQualifier importedMod originatingMod existing
          Hir.DeclSig s -> tryMergeSig s availName.requiresQualifier importedMod originatingMod existing
          _ -> (Nothing, existing)

        updatedModMap = Map.insert importedMod (maybeToList newEntry ++ rest) modMap
        updatedVarMap = Map.insert nameKey updatedModMap currentMap
     in scope {glblVarInfo = updatedVarMap}

  equalSig :: Hir.SigDecl -> Hir.SigDecl -> Bool
  equalSig s1 s2 =
    s1.name.node.nodeText == s2.name.node.nodeText
      && (AST.getDynNode s1.node).nodeText == (AST.getDynNode s2.node).nodeText

  equalBind :: Hir.BindDecl -> Hir.BindDecl -> Bool
  equalBind b1 b2 =
    b1.name.node.nodeText == b2.name.node.nodeText
      && (AST.getDynNode b1.node).nodeText == (AST.getDynNode b2.node).nodeText

  tryMergeSig :: Hir.SigDecl -> Bool -> ImportInfo -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeSig s requiresQualifier importedFrom origMod [] =
    ( Just
        GlblVarInfo
          { sig = Just s
          , binds = []
          , importedFrom = NES.singleton importedFrom
          , originatingMod = origMod
          , name = s.name
          , loc = (AST.getDynNode s.node).nodeLineColRange
          , requiresQualifier
          }
    , []
    )
  tryMergeSig s requiresQualifier importedFrom origMod (v : vs)
    | v.originatingMod == origMod
        && v.name == s.name
        && requiresQualifier == v.requiresQualifier =
        case v.sig of
          Nothing ->
            let merged = v {sig = Just s, importedFrom = NES.insert importedFrom v.importedFrom, requiresQualifier}
             in (Just merged, vs)
          Just existingSig
            | equalSig existingSig s ->
                -- Same sig, skip insert
                (Nothing, v : vs)
            | otherwise ->
                -- Different sig, preserve both
                let (result, rest) = tryMergeSig s requiresQualifier importedFrom origMod vs
                 in (result, v : rest)
    | otherwise =
        let (result, rest) = tryMergeSig s requiresQualifier importedFrom origMod vs
         in (result, v : rest)

  tryMergeBind :: Hir.BindDecl -> Bool -> ImportInfo -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeBind b requiresQualifier importedFrom origMod [] =
    ( Just
        GlblVarInfo
          { sig = Nothing
          , binds = [b]
          , importedFrom = NES.singleton importedFrom
          , originatingMod = origMod
          , name = b.name
          , loc = (AST.getDynNode b.node).nodeLineColRange
          , requiresQualifier
          }
    , []
    )
  tryMergeBind b requiresQualifier importedFrom origMod (v : vs)
    | v.originatingMod == origMod
        && v.name == b.name
        && requiresQualifier == v.requiresQualifier =
        case v.binds of
          [] ->
            let merged = v {binds = [b], importedFrom = NES.insert importedFrom v.importedFrom, loc = (AST.getDynNode b.node).nodeLineColRange}
             in (Just merged, vs)
          [existingBind]
            | equalBind existingBind b ->
                -- Same bind, skip insert
                (Nothing, v : vs)
          _ ->
            -- Different or multiple binds, keep both
            let (result, rest) = tryMergeBind b requiresQualifier importedFrom origMod vs
             in (result, v : rest)
    | otherwise =
        let (result, rest) = tryMergeBind b requiresQualifier importedFrom origMod vs
         in (result, v : rest)
