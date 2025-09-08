module Arborist.FilesSpec (spec) where

import Data.List
import System.Directory (makeRelativeToCurrentDirectory)
import Arborist.Files
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Hir.Types (ModuleText (..))
import Test.Hspec
import Test.Hspec.Golden (defaultGolden)
import TestImport

spec :: Spec
spec = do
  describe "modFileMap" $ do
    describe "golden" $ do
      it "maps module names to their file paths" $ do
        mods <- buildModuleFileMap allTestLibs
        renderedPairs <-
          mapM
            ( \(ModuleText {text}, path) -> do
                rel <- makeRelativeToCurrentDirectory path
                let rel' = if take 2 rel == "./" then rel else "./" <> rel
                pure (T.concat [text, " -> ", T.pack rel'])
            )
            (sortOn (\(ModuleText {text}, _) -> text) (Map.toList mods))
        let renderedModMap = T.unlines renderedPairs
        pure $ defaultGolden "buildModuleFileMap" (T.unpack renderedModMap)
