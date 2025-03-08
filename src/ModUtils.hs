module ModUtils where

import Hir.Types
import qualified Hir.Parse as Hir
import System.FilePath
import qualified Data.Path as Path
import Control.Monad
import qualified Data.Text as T

pathToModule :: [Path.AbsPath] -> Path.AbsPath -> Maybe ModuleText
pathToModule srcDirs absPath = do
  let fp = Path.toFilePath absPath
  modPath <- msum ((\srcDir -> makeRelativeMaybe (Path.toFilePath srcDir) fp) <$> srcDirs)
  let (modPathWithoutExt, ext) = splitExtension modPath
  guard $ ext == ".hs"
  let modText = T.replace (T.pack [pathSeparator]) "." (T.pack modPathWithoutExt)
  pure $ Hir.parseModuleTextFromText modText

modToFilePath :: ModuleText -> String -> Path.RelPath
modToFilePath mod ext =
  Path.filePathToRel (dotsToSlashes (T.unpack mod.text) -<.> ext)
  where dotsToSlashes = map (\c -> if c == '.' then pathSeparator else c)

makeRelativeMaybe :: FilePath -> FilePath -> Maybe FilePath
makeRelativeMaybe base path = do
  let rel = makeRelative base path
  _ <- guard $ path /= rel
  pure rel

