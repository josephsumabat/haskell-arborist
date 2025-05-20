module Arborist.FilesSpec (spec) where

import TestImport
import Test.Hspec
import Arborist.Files
import Control.Monad.IO.Class
import Test.Hspec.Golden (defaultGolden)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Hir.Parse
import Hir.Types (ModuleText(..))

spec :: Spec
spec = do 
  describe "modFileMap" $ do
    it "maps module names to their file paths" $ do
      mods <- buildModuleFileMap allTestLibs
      let modsText = T.unlines $ map (\(ModuleText {text}, path) -> 
            T.concat [text, " -> ", T.pack path]) 
            (Map.toList mods)
      pure $ defaultGolden "buildModuleFileMap" (T.unpack modsText)
