module Scripts.DumpRenamedAst (
  DumpRenamedAstOptions (..),
  runDumpRenamedAst,
) where

import AST.Haskell.Generated
import Arborist.Config
import Arborist.Debug (debugTree)
import Arborist.Files (buildModuleFileMap)
import Arborist.ProgramIndex (gatherScopeDeps)
import Arborist.Renamer (renamePrg)
import Arborist.Scope.Global (ExportIndex)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO
import HaskellAnalyzer (parsePrg)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory)

data DumpRenamedAstOptions = DumpRenamedAstOptions
  { sourceFile :: FilePath
  , outputFile :: Maybe FilePath
  , configFile :: FilePath
  }

runDumpRenamedAst :: DumpRenamedAstOptions -> IO ()
runDumpRenamedAst DumpRenamedAstOptions {sourceFile, outputFile, configFile} = do
  ArboristConfig {srcDirs, immutableSrcDirs} <- loadArboristConfig (Just configFile)
  let sourceRoots = srcDirs ++ immutableSrcDirs
  modFileMap <- buildModuleFileMap sourceRoots

  sourceAbs <- Dir.makeAbsolute sourceFile
  exists <- Dir.doesFileExist sourceAbs
  unless exists $ fail $ "Source file not found: " <> sourceAbs

  fileContents <- TE.decodeUtf8 <$> BS.readFile sourceAbs
  let (_src, prg) = parsePrg fileContents

  programIndex <- gatherScopeDeps Map.empty prg modFileMap Nothing
  let exportIndex :: ExportIndex
      exportIndex = Map.empty

  case renamePrg programIndex exportIndex prg of
    Nothing -> fail "Failed to compute renamed AST for provided source file"
    Just renamedAst -> do
      let debugOutput = debugTree renamedAst.dynNode
      case outputFile of
        Nothing -> putStr debugOutput
        Just target -> do
          outputAbs <- Dir.makeAbsolute target
          Dir.createDirectoryIfMissing True (takeDirectory outputAbs)
          TLIO.writeFile outputAbs (TL.pack debugOutput)
