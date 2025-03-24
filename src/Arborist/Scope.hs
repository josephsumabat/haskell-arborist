{-# LANGUAGE TupleSections #-}

module Arborist.Scope where

import AST
import AST.Haskell qualified as AST
import Control.Error (headMay)
import Control.Monad
import Data.HashMap.Lazy qualified as Map
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Debug.Trace
import HaskellAnalyzer
import Hir
import Hir.Parse qualified as Hir
import Hir.Types (Decl, ModuleText)
import Hir.Types qualified as Hir
import ModUtils
import System.Directory qualified as Dir
import System.FilePath qualified as Dir

data NameInfo = NameInfo
  { name :: T.Text
  , decl :: Decl
  , dynNode :: DynNode
  , originatingMod :: ModuleText
  , importedFrom :: ModuleText
  , requiresQualifier :: Bool
  }
  deriving (Show)

data VarType
  = VarSig Hir.SigDecl
  | VarTy

data GlblVarInfo = GlblVarInfo
  { sig :: Maybe Hir.SigDecl
  , binds :: [Hir.BindDecl]
  , importedFrom :: ModuleText
  , originatingMod :: ModuleText
  }
  deriving (Show)

data LocalVarInfo = LocalVarInfo
  { dynNode :: Node
  }
  deriving (Show)

data VarInfo
  = VarInfoGlobal GlblVarInfo
  | VarInfoLocal LocalVarInfo
  deriving (Show)

-- | Var infos for a name indexed by module
type GlblVarInfoMap = Map.HashMap T.Text ModVarInfoMap

type ModVarInfoMap = Map.HashMap ModuleText [GlblVarInfo]

data Scope = Scope
  { glblVarInfo :: GlblVarInfoMap
  , lclVarInfo :: Map.HashMap T.Text LocalVarInfo
  }
  deriving (Show)

type ProgramIndex = Map.HashMap ModuleText Hir.Program

-- | Nodes which change the scope
type ScopeChanger = AST.HaskellP AST.:+ AST.FunctionP AST.:+ AST.Nil

emptyScope :: Scope
emptyScope =
  Scope
    { glblVarInfo = Map.empty
    , lclVarInfo = Map.empty
    }

getScope :: ProgramIndex -> AST.DynNode -> [Scope] -> [Scope]
getScope availPrgs n !scopeStack =
  case AST.cast @ScopeChanger n of
    Just (AST.Inj @(AST.HaskellP) haskellNode) ->
      -- Add top level and imported bindings
      let (_, prg) = Hir.parseHaskell haskellNode
          availableNames = getAvailableNames availPrgs prg
          modScope = availableNamesToScope availableNames
       in modScope : scopeStack
    Just (AST.Inj @(AST.FunctionP) fnNode) ->
      -- Add local params when encountering a function node
      let params = (Hir.parseFunction fnNode).params
          curScope = fromMaybe emptyScope (headMay scopeStack)
       in List.foldl' addParam curScope params : scopeStack
    Just _ -> scopeStack
    Nothing -> scopeStack

addParam :: Scope -> Hir.Param -> Scope
addParam scope param =
  case param of
    Hir.ParamVar var ->
      let lclVarInfo =
            LocalVarInfo
              { dynNode = var.dynNode
              }
       in scope {lclVarInfo = Map.insert var.name lclVarInfo scope.lclVarInfo}
    _ -> scope

-- | Get reachable programs
-- TODO: handle module re-exports
getRequiredScopePrograms :: Hir.Program -> [FilePath] -> IO ProgramIndex
getRequiredScopePrograms thisPrg srcDirs = do
  let requiredFilesInfo =
        (\imp -> (imp.mod, (moduleToPath ".hs" . (.mod) $ imp)))
          <$> thisPrg.imports
      requiredFilesWithSrc =
        requiredFilesInfo >>= filesWithSrc
  Map.fromList <$> getPrgs requiredFilesWithSrc
 where
  getPrgs :: [(ModuleText, FilePath)] -> IO [(ModuleText, Hir.Program)]
  getPrgs hsFiles =
    fmap catMaybes $
      forM hsFiles $ \(modText, file) -> do
        fileExists <-
          Dir.doesFileExist file
        if fileExists
          then do
            traceShowM $ "parsing: " <> file
            fileContents <- Utf8.readFile file
            let v = parsePrg fileContents
            pure $ Just (modText, snd v)
          else pure Nothing

  filesWithSrc :: (ModuleText, FilePath) -> [(ModuleText, FilePath)]
  filesWithSrc (modText, noSrcPath) =
    (\srcDir -> (modText, srcDir Dir.</> noSrcPath)) <$> srcDirs

prgsToMap :: [Hir.Program] -> ProgramIndex
prgsToMap prgs =
  Map.fromList $
    mapMaybe
      ( \prg ->
          (,prg) <$> prg.mod
      )
      prgs

declToNameInfo :: ModuleText -> ModuleText -> Bool -> Decl -> NameInfo
declToNameInfo originatingMod importedFrom qualified decl =
  NameInfo
    { name = (declName decl).node.nodeText
    , dynNode = declDynNode decl
    , originatingMod = originatingMod
    , importedFrom = importedFrom
    , requiresQualifier = qualified
    , decl = decl
    }

getNamesFromImport :: ProgramIndex -> Hir.Import -> [NameInfo]
getNamesFromImport availPrgs thisImport =
  let qualified = thisImport.qualified
      foundImport = Map.lookup thisImport.mod availPrgs
   in case foundImport of
        Nothing -> []
        Just i -> getExportedNames i qualified

getExportedNames :: Hir.Program -> Bool -> [NameInfo]
getExportedNames prg qualified =
  let declaredInImport modName = fmap (declToNameInfo modName modName qualified) prg.decls
      reExported = []
   in case prg.mod of
        Nothing -> []
        Just modName -> declaredInImport modName <> reExported

getAvailableNames :: ProgramIndex -> Hir.Program -> [NameInfo]
getAvailableNames availPrgs thisPrg =
  let declaredNames =
        case thisPrg.mod of
          Nothing -> []
          Just m -> fmap (declToNameInfo m m False) thisPrg.decls
      importedNames = getNamesFromImport availPrgs =<< thisPrg.imports
   in declaredNames <> importedNames

availableNamesToScope :: [NameInfo] -> Scope
availableNamesToScope availNames = List.foldl' indexNameInfo emptyScope availNames
 where
  indexNameInfo :: Scope -> NameInfo -> Scope
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

  getVarInfos :: T.Text -> ModuleText -> Map.HashMap T.Text (Map.HashMap ModuleText [GlblVarInfo]) -> [GlblVarInfo]
  getVarInfos nameKey moduleKey varMap =
    Map.findWithDefault [] moduleKey $
      Map.findWithDefault Map.empty nameKey varMap

  tryMergeBind :: Hir.BindDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeBind b importedFrom origMod [] =
    (Just (GlblVarInfo {sig = Nothing, binds = [b], importedFrom, originatingMod = origMod}), [])
  tryMergeBind b importedFrom origMod (v : vs)
    | null v.binds =
        let merged = v {binds = [b], importedFrom}
         in (Just merged, vs)
    | otherwise =
        let (result, rest) = tryMergeBind b importedFrom origMod vs
         in (result, v : rest)

  tryMergeSig :: Hir.SigDecl -> ModuleText -> ModuleText -> [GlblVarInfo] -> (Maybe GlblVarInfo, [GlblVarInfo])
  tryMergeSig s importedFrom origMod [] =
    (Just (GlblVarInfo {sig = Just s, binds = [], importedFrom, originatingMod = origMod}), [])
  tryMergeSig s importedFrom origMod (v : vs)
    | isNothing v.sig =
        let merged = v {sig = Just s, importedFrom}
         in (Just merged, vs)
    | otherwise =
        let (result, rest) = tryMergeSig s importedFrom origMod vs
         in (result, v : rest)
