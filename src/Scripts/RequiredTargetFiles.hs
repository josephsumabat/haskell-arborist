module Scripts.RequiredTargetFiles (RequiredTargetFilesOptions (..), runRequiredTargetFiles) where

import Arborist.Config (allSourceRoots, loadArboristConfig)
import Arborist.Files (buildModuleFileMap)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.HashMap.Lazy qualified as HM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import HaskellAnalyzer (parsePrg)
import Hir.Parse qualified as Hir
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as HirT

-- | Options for the required-target-files command
--   configPath: optional override for the Arborist configuration file
--   moduleNames: at least one module whose direct import files will be listed
data RequiredTargetFilesOptions = RequiredTargetFilesOptions
  { configPath :: Maybe FilePath
  , moduleNames :: NonEmpty Text
  }

runRequiredTargetFiles :: RequiredTargetFilesOptions -> IO ()
runRequiredTargetFiles RequiredTargetFilesOptions {configPath, moduleNames} = do
  config <- loadArboristConfig configPath
  let sourceRoots = allSourceRoots config
  modFileMap <- buildModuleFileMap sourceRoots

  requiredFilesPerModule <- mapM (gatherRequiredFiles modFileMap) (NE.toList moduleNames)
  let encoded = HM.fromList requiredFilesPerModule
  BL8.putStrLn (Aeson.encode encoded)

-- | Collect the direct import file paths for a module, if it can be resolved.
gatherRequiredFiles :: HM.HashMap HirT.ModuleText FilePath -> Text -> IO (Text, Maybe (HM.HashMap Text (Maybe FilePath)))
gatherRequiredFiles modFileMap moduleNameText = do
  let moduleText = Hir.parseModuleTextFromText moduleNameText
  case HM.lookup moduleText modFileMap of
    Nothing -> pure (moduleNameText, Nothing)
    Just moduleFile -> do
      program <- parseProgramFromFile moduleFile
      let importedModules = (.mod) <$> HirT.getImports program
          resolved =
            HM.fromList
              [ (targetMod.text, HM.lookup targetMod modFileMap)
              | targetMod <- importedModules
              ]
      pure (moduleNameText, Just resolved)

parseProgramFromFile :: FilePath -> IO Hir.Read.Program
parseProgramFromFile filePath = do
  contents <- TE.decodeUtf8 <$> BS.readFile filePath
  let (_source, program) = parsePrg contents
  pure program
