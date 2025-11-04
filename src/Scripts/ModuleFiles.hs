module Scripts.ModuleFiles (ModuleFilesOptions (..), runModuleFiles) where

import Arborist.Config (allSourceRoots, loadArboristConfig)
import Arborist.Files (buildModuleFileMap)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.HashMap.Lazy qualified as HM
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Hir.Parse qualified as Hir

data ModuleFilesOptions = ModuleFilesOptions
  { configPath :: Maybe FilePath
  , moduleNames :: NonEmpty Text
  }

runModuleFiles :: ModuleFilesOptions -> IO ()
runModuleFiles ModuleFilesOptions {configPath, moduleNames} = do
  config <- loadArboristConfig configPath
  fileMap <- buildModuleFileMap (allSourceRoots config)
  let resolved :: HM.HashMap Text (Maybe FilePath)
      resolved =
        HM.fromList
          [ let moduleText = Hir.parseModuleTextFromText moduleName
             in (moduleName, HM.lookup moduleText fileMap)
          | moduleName <- NE.toList moduleNames
          ]
  BL8.putStrLn (Aeson.encode resolved)
