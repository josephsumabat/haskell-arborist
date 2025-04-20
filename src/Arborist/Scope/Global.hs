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
import GHC.Stack
import Hir
import Hir.Parse
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

-- Public API wrappers
getExportedNames :: (HasCallStack) => ProgramIndex -> ExportIndex -> ModuleText -> ([ExportedName], ExportIndex)
getExportedNames prgIdx exportIdx modName =
  getExportedNames' prgIdx exportIdx Set.empty modName

getManyImportNames :: ProgramIndex -> ExportIndex -> [Hir.Import] -> ([GlblNameInfo], ExportIndex)
getManyImportNames prgIdx exportIdx imps =
  getManyImportNames' prgIdx exportIdx Set.empty imps

getImportNames :: (HasCallStack) => ProgramIndex -> ExportIndex -> Hir.Import -> ([GlblNameInfo], ExportIndex)
getImportNames prgIdx exportIdx imp =
  getImportNames' prgIdx exportIdx Set.empty imp

-- Internal cycle-safe versions
getImportNames' ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  Hir.Import ->
  ([GlblNameInfo], ExportIndex)
getImportNames' prgIndex exportIndex inProgress thisImport =
  let qualified = thisImport.qualified
      mod = thisImport.mod
      (exportedNames, updatedExportIndex) = getExportedNames' prgIndex exportIndex inProgress mod
      glblNameInfo = exportToInfo mod qualified <$> exportedNames
      importNames = Set.fromList $ (.nodeText) . (.node) . (.name) <$> thisImport.importList
      importList
        | null importNames = glblNameInfo
        | thisImport.hiding = filter (\n -> not (Set.member n.name importNames)) glblNameInfo
        | otherwise = filter (\n -> Set.member n.name importNames) glblNameInfo
   in (importList, updatedExportIndex)

getManyImportNames' ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  [Hir.Import] ->
  ([GlblNameInfo], ExportIndex)
getManyImportNames' prgIndex exportIndex inProgress imports =
  List.foldl'
    ( \(importedNamesAgg, idx) imp ->
        let (importedNames, nextIdx) = getImportNames' prgIndex idx inProgress imp
         in (importedNames <> importedNamesAgg, nextIdx)
    )
    ([], exportIndex)
    imports

getTransitiveReExports :: Hir.Program -> [Hir.ExportItem] -> Set.Set T.Text
getTransitiveReExports prg exports =
  let declaredNames = fmap declNameText prg.decls
      declaredNamesSet = Set.fromList declaredNames
      exportNamesSet = Set.fromList $ (.node.nodeText) <$> exportItemNames exports
   in exportNamesSet `Set.difference` declaredNamesSet

getExportedNames' ::
  (HasCallStack) =>
  ProgramIndex ->
  ExportIndex ->
  Set.Set ModuleText ->
  ModuleText ->
  ([ExportedName], ExportIndex)
getExportedNames' prgIndex exportIndex inProgress modName
  | modName `Set.member` inProgress = ([], exportIndex)
  | Just exports <- Map.lookup modName exportIndex = (exports, exportIndex)
  | Just prg <- Map.lookup modName prgIndex = getExportedNamesFromPrg prg
  | otherwise = ([], exportIndex)
 where
  getExportedNamesFromPrg :: (HasCallStack) => Hir.Program -> ([ExportedName], ExportIndex)
  getExportedNamesFromPrg prg =
    let declaredNames = getDeclaredNames modName prg
        inProgress' = Set.insert modName inProgress
     in case prg.exports of
          Nothing ->
            let exportIdxWithSelf = Map.insert modName declaredNames exportIndex
             in (declaredNames, exportIdxWithSelf)
          Just exportLst ->
            let
              exportNamesSet = Set.fromList $ (.node.nodeText) <$> exportItemNames exportLst
              transitiveReexportNames = getTransitiveReExports prg exportLst
              reExportedMods = Set.fromList $ (.mod) <$> exportItemMods exportLst
              requiredImports =
                if null transitiveReexportNames
                  then filter (\imp -> imp.mod `Set.member` reExportedMods) prg.imports
                  else prg.imports

              (allImportedNames, updatedExportIdx) =
                getManyImportNames' prgIndex exportIndex inProgress' requiredImports

              transitiveReexportNamesInfo =
                filter
                  ( \expInfo ->
                      expInfo.name `Set.member` exportNamesSet
                        || expInfo.importedFrom `Set.member` reExportedMods
                  )
                  allImportedNames

              moduleExports =
                filter
                  ( \expInfo ->
                      expInfo.importedFrom `Set.member` reExportedMods
                  )
                  allImportedNames

              exportedNames =
                (infoToExport <$> moduleExports)
                  <> (infoToExport <$> transitiveReexportNamesInfo)
                  <> filter (\expInfo -> expInfo.name `Set.member` exportNamesSet) declaredNames

              updateExportIdxWithSelf = Map.insert modName exportedNames updatedExportIdx
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

  equalSig :: Hir.SigDecl -> Hir.SigDecl -> Bool
  equalSig s1 s2 =
    s1.name.node.nodeText == s2.name.node.nodeText
      && (AST.getDynNode s1.node).nodeText == (AST.getDynNode s2.node).nodeText

  equalBind :: Hir.BindDecl -> Hir.BindDecl -> Bool
  equalBind b1 b2 =
    b1.name.node.nodeText == b2.name.node.nodeText
      && (AST.getDynNode b1.node).nodeText == (AST.getDynNode b2.node).nodeText

  tryMergeSig :: Hir.SigDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeSig s importedFrom origMod [] =
    ( Just
        GlblVarInfo
          { sig = Just s
          , binds = []
          , importedFrom
          , originatingMod = origMod
          , name = s.name
          , loc = (AST.getDynNode s.node).nodeLineColRange
          }
    , []
    )
  tryMergeSig s importedFrom origMod (v : vs)
    | v.originatingMod == origMod
        && v.name.node.nodeText == s.name.node.nodeText =
        case v.sig of
          Nothing ->
            let merged = v {sig = Just s, importedFrom}
             in (Just merged, vs)
          Just existingSig
            | equalSig existingSig s ->
                -- Same sig, skip insert
                (Nothing, v : vs)
            | otherwise ->
                -- Different sig, preserve both
                let (result, rest) = tryMergeSig s importedFrom origMod vs
                 in (result, v : rest)
    | otherwise =
        let (result, rest) = tryMergeSig s importedFrom origMod vs
         in (result, v : rest)

  tryMergeBind :: Hir.BindDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeBind b importedFrom origMod [] =
    ( Just
        GlblVarInfo
          { sig = Nothing
          , binds = [b]
          , importedFrom
          , originatingMod = origMod
          , name = b.name
          , loc = (AST.getDynNode b.node).nodeLineColRange
          }
    , []
    )
  tryMergeBind b importedFrom origMod (v : vs)
    | v.originatingMod == origMod
        && v.name.node.nodeText == b.name.node.nodeText =
        case v.binds of
          [] ->
            let merged = v {binds = [b], importedFrom, loc = (AST.getDynNode b.node).nodeLineColRange}
             in (Just merged, vs)
          [existingBind]
            | equalBind existingBind b ->
                -- Same bind, skip insert
                (Nothing, v : vs)
          _ ->
            -- Different or multiple binds, keep both
            let (result, rest) = tryMergeBind b importedFrom origMod vs
             in (result, v : rest)
    | otherwise =
        let (result, rest) = tryMergeBind b importedFrom origMod vs
         in (result, v : rest)
