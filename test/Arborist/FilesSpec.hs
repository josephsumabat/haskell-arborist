module Arborist.FilesSpec (spec) where

import TestImport
import Test.Hspec
import Arborist.Files
import Control.Monad.IO.Class

spec :: Spec
spec = do 
  describe "modFileMap" $ do
    it "test" $ do
      mods <- buildModuleFileMap allTestLibs
      pure ()
