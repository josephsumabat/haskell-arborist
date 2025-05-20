{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Arborist.ProgramIndexSpec (spec) where

import Test.Hspec
import Arborist.ProgramIndex
import Arborist.Files
import qualified Data.HashMap.Strict as Map
import Hir.Parse
import TestImport
import System.FilePath
import Test.Hspec.Golden (defaultGolden)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashSet as HashSet
import Data.ByteString qualified as BS
import HaskellAnalyzer

spec :: Spec
spec = do
  describe "getPrgs" $ do
    describe "golden" $ do
      it "works with no initial map" $ do
        let mods =
              [
                (parseModuleTextFromText "Data.List"
                , testDataDir </> "Data/List.hs")
              , (parseModuleTextFromText "Data.IORef"
                , testDataDir </> "Data/IORef.hs")
              ]
        prgs <- getPrgs Map.empty mods
        pure $ defaultGolden "getPrgs_no_initial_map" (show prgs)

  describe "gatherScopeDeps" $ do
    it "includes target module and its direct imports at depth 0" $ do
      modFileMap <- buildModuleFileMap allTestLibs
      let Just applicativeFile = Map.lookup (parseModuleTextFromText "Control.Applicative") modFileMap
      applicativeContents <- BS.readFile applicativeFile
      let (_, applicativePrg) = parsePrg (decodeUtf8 applicativeContents)
      deps <- gatherScopeDeps Map.empty applicativePrg modFileMap (Just 0)
      let expectedModules = HashSet.fromList 
            [ parseModuleTextFromText "Control.Applicative"
            , parseModuleTextFromText "Control.Arrow"
            , parseModuleTextFromText "Control.Category"
            , parseModuleTextFromText "Data.Monoid"
            , parseModuleTextFromText "Control.Monad.ST.Safe"
            , parseModuleTextFromText "Data.Proxy"
            , parseModuleTextFromText "Data.Functor"
            , parseModuleTextFromText "Control.Monad"
            , parseModuleTextFromText "Control.Monad.ST.Lazy.Safe"
            ]
      Map.keysSet deps `shouldBe` expectedModules

    it "includes target module, direct imports, and their imports at depth 1" $ do
      modFileMap <- buildModuleFileMap allTestLibs
      let Just applicativeFile = Map.lookup (parseModuleTextFromText "Control.Applicative") modFileMap
      applicativeContents <- BS.readFile applicativeFile
      let (_, applicativePrg) = parsePrg (decodeUtf8 applicativeContents)
      deps <- gatherScopeDeps Map.empty applicativePrg modFileMap (Just 1)
      let expectedModules = HashSet.fromList 
            [ parseModuleTextFromText "Control.Applicative"
            , parseModuleTextFromText "Control.Arrow"
            , parseModuleTextFromText "Control.Category"
            , parseModuleTextFromText "Data.Monoid"
            , parseModuleTextFromText "Data.Maybe"
            , parseModuleTextFromText "Control.Monad.ST.Safe"
            , parseModuleTextFromText "Data.Proxy"
            , parseModuleTextFromText "Data.Functor"
            , parseModuleTextFromText "Control.Monad.ST.Imp"
            , parseModuleTextFromText "Control.Monad"
            , parseModuleTextFromText "Control.Monad.ST.Lazy.Safe"
            , parseModuleTextFromText "Control.Monad.Fix"
            , parseModuleTextFromText "Control.Monad.ST.Lazy.Imp"
            ]
      Map.keysSet deps `shouldBe` expectedModules

    it "includes all transitive dependencies when no max depth is specified" $ do
      modFileMap <- buildModuleFileMap allTestLibs
      let Just applicativeFile = Map.lookup (parseModuleTextFromText "Control.Applicative") modFileMap
      applicativeContents <- BS.readFile applicativeFile
      let (_, applicativePrg) = parsePrg (decodeUtf8 applicativeContents)
      deps <- gatherScopeDeps Map.empty applicativePrg modFileMap Nothing
      let expectedModules = HashSet.fromList 
            [ parseModuleTextFromText "Control.Applicative"
            , parseModuleTextFromText "Control.Arrow"
            , parseModuleTextFromText "Control.Category"
            , parseModuleTextFromText "Data.Type.Coercion"
            , parseModuleTextFromText "Data.Monoid"
            , parseModuleTextFromText "Data.Type.Bool"
            , parseModuleTextFromText "Data.Maybe"
            , parseModuleTextFromText "Control.Monad.ST.Safe"
            , parseModuleTextFromText "Data.Proxy"
            , parseModuleTextFromText "Data.Functor"
            , parseModuleTextFromText "Control.Monad.ST.Imp"
            , parseModuleTextFromText "Control.Monad"
            , parseModuleTextFromText "Control.Monad.ST.Lazy.Safe"
            , parseModuleTextFromText "Data.Function"
            , parseModuleTextFromText "Data.Type.Equality"
            , parseModuleTextFromText "Control.Monad.Fix"
            , parseModuleTextFromText "Data.Bool"
            , parseModuleTextFromText "Control.Monad.ST.Unsafe"
            , parseModuleTextFromText "Control.Monad.ST.Lazy.Imp"
            ]
      Map.keysSet deps `shouldBe` expectedModules
