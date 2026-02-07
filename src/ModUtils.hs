module ModUtils where

import Control.Monad
import Data.HashMap.Lazy qualified as Map
import Data.Path qualified as Path
import Data.Text qualified as T
import Hir.Parse qualified as Hir
import Hir.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath

pathToModule :: [Path.AbsPath] -> Path.AbsPath -> Maybe ModuleText
pathToModule srcDirs absPath = do
  let fp = Path.toFilePath absPath
  unsafePathToModule (Path.toFilePath <$> srcDirs) fp

unsafePathToModule :: [FilePath] -> FilePath -> Maybe ModuleText
unsafePathToModule srcDirs fp = do
  modPath <- msum ((\srcDir -> makeRelativeMaybe srcDir fp) <$> srcDirs)
  let (modPathWithoutExt, ext) = splitExtension modPath
  guard $ ext == ".hs"
  let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
  pure $ Hir.parseModuleTextFromText modText

moduleToPath :: String -> ModuleText -> FilePath
moduleToPath ext mod =
  (dotsToSlashes (T.unpack mod.text) -<.> ext)
 where
  dotsToSlashes = map (\c -> if c == '.' then pathSeparator else c)

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  _ <- guard $ path /= rel
  pure rel

modWithFiles :: [FilePath] -> ModuleText -> [(ModuleText, FilePath)]
modWithFiles baseDirs modText =
  filesWithSrc (modText, (moduleToPath ".hs" modText))
 where
  filesWithSrc :: (ModuleText, FilePath) -> [(ModuleText, FilePath)]
  filesWithSrc (modText, noSrcPath) =
    (\srcDir -> (modText, srcDir </> noSrcPath)) <$> baseDirs
