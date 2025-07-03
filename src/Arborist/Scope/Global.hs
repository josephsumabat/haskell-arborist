module Arborist.Scope.Global (
  getExportedDecls,
  getImportDecls,
  globalDeclsToScope,
  getGlobalAvalibleDecls,
  ExportIndex,
  declToExportedName,
  exportToInfo,
  infoToExport,
)
where

import Arborist.Debug.Trace
import AST
import AST.Haskell qualified as AST
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
import Data.List.NonEmpty qualified as NE
import AST.Haskell qualified as H
import Data.Either.Extra (eitherToMaybe)
import Data.List.NonEmpty qualified as NE
import AST.Extension (ParsePhase)
import AST.Runtime
import Hir.Parse
import AST.Unwrap qualified

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

-- TODO: Instead of removing an import if it is hidden, just tag it instead of excluding it, when we are doing the renaming make sure the import is avalible to be used
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
      importInfo = ImportInfo { mod = mod, namespace = alias }
      glblNameInfo = exportToInfo importInfo qualified <$> exportedNames
    in (glblNameInfo, updatedExportIndex)

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

-- this brings all possible decls into scope including hidden ones (but tagged), add tag
getGlobalAvalibleDecls :: ProgramIndex -> ExportIndex -> Hir.Program -> [GlblDeclInfo]
getGlobalAvalibleDecls availPrgs exportIdx thisPrg =
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

declToNameInfo :: GlblDeclInfo -> Maybe GlblNameInfo
declToNameInfo d =
  case d.decl of
    Hir.DeclData dd -> Just (mk DataDecl (dd.name) (dd.node))
    Hir.DeclNewtype nt -> Just (mk NewtypeDecl (nt.name) (nt.node))
    Hir.DeclClass cl -> Just (mk ClassDecl (cl.name) (cl.node))
    Hir.DeclDataFamily df -> Just (mk DataFamilyDecl (df.name) (df.node))
    Hir.DeclTypeFamily tf -> Just (mk TypeFamilyDecl (tf.name) (tf.node))
    Hir.DeclTypeSynonym ts -> Just (mk TypeSynonymDecl (ts.name) (ts.node))
    _ -> Nothing
 where
  mk kind actualName astNode =
    let nodeInfo = getDynNode astNode
     in GlblNameInfo
          { name = actualName
          , importedFrom = NES.singleton d.importedFrom
          , originatingMod = d.originatingMod
          , loc = nodeInfo.nodeLineColRange
          , requiresQualifier = d.requiresQualifier
          , decl = d.decl
          , nameKind = kind
          }

-- TODO filter hidden here instead
-- | From a list of annotated declarations, attempt to build a scope - will try to
-- merge associated declarations together (e.g. a type signature and multiple binds)
globalDeclsToScope :: [GlblDeclInfo] -> [Hir.Import] -> Scope
globalDeclsToScope availDecl imports = 
  let filteredDecl = filterHiddenItems availDecl imports
  in List.foldl' indexDeclInfo emptyScope filteredDecl
 where
  filterHiddenItems :: [GlblDeclInfo] -> [Hir.Import] -> [GlblDeclInfo]
  filterHiddenItems decls imps = 
    filter (not . isHidden imps) decls
    
  isHidden :: [Hir.Import] -> GlblDeclInfo -> Bool
  isHidden imps declInfo = 
    case findMatchingImport imps declInfo of
      Nothing -> False
      Just imp -> shouldBeHidden imp declInfo
      
  findMatchingImport :: [Hir.Import] -> GlblDeclInfo -> Maybe Hir.Import
  findMatchingImport imps declInfo = List.find (\imp -> imp.mod == declInfo.importedFrom.mod) imps
    
  shouldBeHidden :: Hir.Import -> GlblDeclInfo -> Bool  
  shouldBeHidden imp declInfo =
    let importNames = Set.fromList $ (.nodeText) . (.node) . (.name) <$> imp.importList
    in if null importNames
        then False
       else if imp.hiding
        then Set.member declInfo.name importNames
       else 
          not (Set.member declInfo.name importNames)

  indexDeclInfo :: Scope -> GlblDeclInfo -> Scope
  indexDeclInfo scope availDecl =
    let declKey = availDecl.name
        originatingMod = availDecl.originatingMod
        importedMod = availDecl.importedFrom
        moduleKey = importedMod
        currentMap = scope.glblVarInfo
        modMap = Map.findWithDefault Map.empty declKey currentMap
        existing = Map.findWithDefault [] moduleKey modMap

        (newEntry, rest) = case availDecl.decl of
          Hir.DeclBind b -> tryMergeBind b availDecl.requiresQualifier importedMod originatingMod existing
          Hir.DeclSig s -> tryMergeSig s availDecl.requiresQualifier importedMod originatingMod existing
          _ -> (Nothing, existing)

        updatedModMap = Map.insert importedMod (maybeToList newEntry ++ rest) modMap
        updatedVarMap = Map.insert declKey updatedModMap currentMap

        -- decl to maybe Name
        mNameInfo = declToNameInfo availDecl
        nameMap = scope.glblNameInfo

        updatedNameMap = case mNameInfo of
          Nothing -> nameMap
          Just nameInfo ->
            let key = nameInfo.name.node.nodeText
                impInfo = importedMod
                nameMap = scope.glblNameInfo
                importMap = Map.findWithDefault Map.empty key nameMap
                existing = Map.findWithDefault [] impInfo importMap
                newImportMap = Map.insert impInfo (existing ++ [nameInfo]) importMap
             in Map.insert key newImportMap nameMap


        constructors = case availDecl.decl of
          Hir.DeclData dataDecl -> extractDataConstructors dataDecl availDecl
          Hir.DeclNewtype newtypeDecl -> extractNewtypeConstructor newtypeDecl availDecl
          _ -> []

        updatedConstructorMap = addConstructorsToMap constructors scope.glblConstructorInfo

      in scope { glblVarInfo = updatedVarMap
                , glblNameInfo = updatedNameMap
                , glblConstructorInfo = updatedConstructorMap
                }


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

  -- once we have all constructors add them to the map
  addConstructorsToMap :: [GlblConstructorInfo] -> GlblConstructorInfoMap -> GlblConstructorInfoMap
  addConstructorsToMap constructors currentMap =
    List.foldl' addSingleConstructor currentMap constructors

  addSingleConstructor :: GlblConstructorInfoMap -> GlblConstructorInfo -> GlblConstructorInfoMap
  addSingleConstructor currentMap constructor =
    let key = constructor.name.node.nodeText
        impInfo = NES.findMin constructor.importedFrom
        importMap = Map.findWithDefault Map.empty key currentMap
        existing = Map.findWithDefault [] impInfo importMap
        newImportMap = Map.insert impInfo (existing ++ [constructor]) importMap
    in Map.insert key newImportMap currentMap

 -- find all constructors

  extractDataConstructors :: Hir.DataDecl -> GlblDeclInfo -> [GlblConstructorInfo]
  extractDataConstructors decl parentInfo =
    let dataTypeNode = decl.node in
        case unwrap dataTypeNode of
          Left _ -> []
          Right H.DataTypeU { constructors = mCons } ->
              case mCons of
              -- regular DataConstructors branch
                Just (AST.Inj @(H.DataConstructorsP) dataConstructor) ->
                    case unwrap dataConstructor of
                      Left _ -> []
                      Right H.DataConstructorsU { constructor = nes } ->
                        concatMap
                          (\dataCon ->
                                let mName = parseDataConName dataCon in
                                maybeToList $ makeConstructorInfo parentInfo dataCon.dynNode <$> mName
                          )
                          (NE.toList nes)
                  -- TODO: the GADT branch
                Just (AST.Inj @H.GadtConstructorsP _gadtConstructor) -> []
                _ -> []

  extractNewtypeConstructor :: Hir.NewtypeDecl -> GlblDeclInfo -> [GlblConstructorInfo]
  extractNewtypeConstructor decl parentInfo =
    case unwrap (decl.node) of
      Right H.NewtypeU { dynNode = constructorDyn, constructor = constructorNode } ->
        let mName = parseNewtypeConName =<< constructorNode in
        maybeToList $ makeConstructorInfo parentInfo constructorDyn <$> mName
      _ -> []

  parseDataConName :: AST.DataConstructorP -> Maybe Hir.Name
  parseDataConName dataConNode =
    case (.constructor) <$> AST.unwrapMaybe dataConNode of
      Just (AST.Inj @H.PrefixP p) ->
        parsePrefix p
      Just (AST.Inj @H.RecordP r) ->
        parseName . AST.Inj <$> (AST.unwrapMaybe r >>= (.name))
      _ -> Nothing

  parseNewtypeConName :: AST.NewtypeConstructorP -> Maybe Hir.Name
  parseNewtypeConName newtypeCon =
    case (.name) <$> AST.unwrapMaybe newtypeCon of
      Just (AST.Inj @H.PrefixIdP p) ->
        eitherToMaybe $ parseNamePrefix (AST.Inj p)
      Just (AST.Inj @H.ConstructorP r) ->
        Just $ parseName (AST.Inj r)
      _ -> Nothing

  makeConstructorInfo :: GlblDeclInfo -> DynNode -> Hir.Name -> GlblConstructorInfo
  makeConstructorInfo parentInfo conNode name =
    GlblConstructorInfo
      { name = name
      , importedFrom = NES.singleton parentInfo.importedFrom
      , originatingMod = parentInfo.originatingMod
      , loc = conNode.nodeLineColRange
      , requiresQualifier = parentInfo.requiresQualifier
      , parentType = parentInfo
      , node = conNode
      }

