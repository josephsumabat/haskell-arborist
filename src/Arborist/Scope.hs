{-# LANGUAGE TupleSections #-}

module Arborist.Scope where

import AST
import Control.Error (fromMaybe)
import Data.HashMap.Lazy qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Hir
import Hir.Types (Decl, ExportItem, ModuleText)
import Hir.Types qualified as Hir
import qualified Data.Set as Set
import Data.Path
import ModUtils
import qualified Data.Text.IO.Utf8 as Utf8
import HaskellAnalyzer
import Control.Monad
import qualified System.Directory as Dir
import qualified System.FilePath as Dir

data NameInfo = NameInfo
  { name :: T.Text
  , dynNode :: DynNode
  , originatingMod :: ModuleText
  , requiresQualifier :: Bool
  }
  deriving (Show)

type AvailableNames = [NameInfo]

type ExportedNames = [NameInfo]

data Scope = Scope
  { availNames :: AvailableNames
  , exportedNames :: ExportedNames
  }

type ImportedPrograms = Map.HashMap ModuleText Hir.Program

-- | Get reachable programs
-- TODO: handle module re-exports
getRequiredScopePrograms :: Hir.Program -> [FilePath] -> IO ImportedPrograms
getRequiredScopePrograms thisPrg srcDirs = do
  let requiredFilesInfo =
        (\imp -> (imp.mod, (moduleToPath ".hs" . (.mod) $ imp)))
        <$> thisPrg.imports
      requiredFilesWithSrc =
        requiredFilesInfo >>= filesWithSrc
  Map.fromList <$> getImportedPrgs requiredFilesWithSrc
    where
    getImportedPrgs :: [(ModuleText, FilePath)] -> IO [(ModuleText, Hir.Program)]
    getImportedPrgs hsFiles =
      fmap catMaybes $
        forM hsFiles $ \(modText, file)-> do
          fileExists <- Dir.doesFileExist file
          if fileExists then do
            fileContents <- Utf8.readFile file
            let v = parsePrg fileContents
            pure $ Just (modText, snd v)
          else
            pure Nothing
    
    filesWithSrc :: (ModuleText, FilePath) -> [(ModuleText, FilePath)]
    filesWithSrc (modText, noSrcPath) =
      (\srcDir -> (modText, srcDir Dir.</> noSrcPath)) <$> srcDirs


prgsToMap :: [Hir.Program] -> ImportedPrograms
prgsToMap prgs =
  Map.fromList $
    mapMaybe
      ( \prg ->
          (,prg) <$> prg.mod
      )
      prgs

declToNameInfo :: ModuleText -> Bool -> Decl -> NameInfo
declToNameInfo modName qualified decl =
  NameInfo
    { name = (declName decl).node.nodeText
    , dynNode = declDynNode decl
    , originatingMod = modName
    , requiresQualifier = qualified
    }

getNamesFromImport :: ImportedPrograms -> Hir.Import -> AvailableNames
getNamesFromImport availPrgs thisImport =
  let qualified = thisImport.qualified
      foundImport = Map.lookup thisImport.mod availPrgs
   in case foundImport of
        Nothing -> []
        Just i -> getExportedNames i qualified

getExportedNames :: Hir.Program -> Bool -> AvailableNames
getExportedNames prg qualified =
  case prg.mod of
    Nothing -> []
    Just modName -> declToNameInfo modName qualified <$> prg.decls

getAvailableNames :: ImportedPrograms -> Hir.Program -> AvailableNames
getAvailableNames availPrgs thisPrg =
  let declaredNames =
        case thisPrg.mod of
          Nothing -> []
          Just m -> declToNameInfo m False <$> thisPrg.decls
      importedNames = getNamesFromImport availPrgs =<< thisPrg.imports
   in declaredNames <> importedNames
