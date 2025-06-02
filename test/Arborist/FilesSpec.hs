module Arborist.FilesSpec (spec) where

import Data.List
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
        let renderedModMap =
              T.unlines $
                map
                  ( \(ModuleText {text}, path) ->
                      T.concat [text, " -> ", T.pack path]
                  )
                  (sortOn fst $ Map.toList mods)
        pure $ defaultGolden "buildModuleFileMap" (T.unpack renderedModMap)
