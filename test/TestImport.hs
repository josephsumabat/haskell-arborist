module TestImport (
  lazyGetPrgs,
  getPrg,
  testDataDir,
  allTestLibs,
  getDecls,
) where

import Arborist.ProgramIndex
import Control.Error (mapMaybe)
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.HashMap.Lazy qualified as Map
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text
import GHC.IO (unsafeInterleaveIO)
import HaskellAnalyzer
import Hir.Types qualified as Hir

testDataDir :: FilePath
testDataDir = "./test-data/base-data"

allTestLibs :: [FilePath]
allTestLibs =
  [ "./test-data/base-control"
  , testDataDir
  ]

lazyGetPrgs :: [FilePath] -> IO ProgramIndex
lazyGetPrgs hsFiles = do
  -- traceShowM hsFiles
  entries <- forM hsFiles $ \file -> do
    prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
    pure (file, prog)
  pure $ Map.fromList $ mapMaybe (\(_, prg) -> (,prg) <$> prg.mod) entries

parseFile :: [Char] -> IO Hir.Program
parseFile file = do
  -- traceShowM $ "parsing: " <> file
  fileContents <- fmap TE.decodeUtf8 . BS.readFile $ file
  pure $ snd (parsePrg fileContents) -- Run `test` function

getPrg :: [FilePath] -> IO [Hir.Program]
getPrg hsFiles =
  forM hsFiles $ \file -> do
    fileContents <- Text.readFile file
    let v = parsePrg fileContents
    pure $ snd v

getDecls :: Hir.Program -> [Hir.Decl]
getDecls prog = prog.decls
