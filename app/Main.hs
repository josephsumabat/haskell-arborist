module Main where

import AST.Cast
import AST.Haskell qualified
import Arborist.Debug
import Arborist.ModGraph
import Arborist.Renamer (renamePrg)
import Arborist.Renamer.GlobalEnv
import Arborist.Scope
import Arborist.Scope.Global
import Control.Monad (forM)
import Criterion.Main
import Criterion.Types (Config (..))
import Data.HashMap.Lazy qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text
import Data.Time
import Debug.Trace
import GHC.IO (unsafeInterleaveIO)
import HaskellAnalyzer
import Hir.Parse
import Hir.Types qualified as Hir
import System.IO
import Text.Pretty.Simple
import UnliftIO (mapConcurrently)

main :: IO ()
main = do
  let src = "../mercury-web-backend/src"
  hsFiles <- getHsFiles src
  -- mapM_ putStrLn hsFiles

  prgs <- lazyGetPrgs hsFiles
  let modIndex = mkModIndex prgs
  let found = findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR" modIndex
  let haskell = fromJust $ cast @AST.Haskell.HaskellP (last prgs).dynNode
  target <- head <$> lazyGetPrgs ["../mercury-web-backend/src/Mobile/Push.hs"]
  (requiredPrograms, exportIdx) <- gatherScopeDeps target [src]
  let modText = parseModuleTextFromText "Mobile.AppLink"
  let debugTreeStr = fromJust $ (debugTree . (.dynNode)) <$> renamePrg requiredPrograms exportIdx target
  let allRenamed = (\prg -> renamePrg (prgsToMap prgs) Map.empty prg) <$> prgs
  let debugAllTreeStr _s = catMaybes $ fmap (debugTree . (.dynNode)) <$> allRenamed

  -- benchmarkMain modIndex debugAllTreeStr prgs

  -- _ <- putStrLn debugTreeStr
  time
    ( withFile "myFile.txt" AppendMode $ \h -> do
        hSetEncoding h utf8 -- Set UTF-8 encoding
        -- hPutStrLn h (Text.unpack (pShowNoColor (head prgs)))
        -- hPutStrLn h (Text.unpack $ pShowNoColor (getAvailableNames requiredPrograms (last prgs)))
        hPutStrLn h (debugTreeStr)
    )
    "abc"

lazyGetPrgs :: [FilePath] -> IO [Hir.Program]
lazyGetPrgs hsFiles = do
  entries <- forM hsFiles $ \file -> do
    prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
    pure (file, prog)
  pure $ snd <$> entries

parseFile :: [Char] -> IO Hir.Program
parseFile file = do
  traceShowM $ "parsing: " <> file
  fileContents <- Text.readFile file
  pure $ snd (parsePrg fileContents) -- Run `test` function

getPrgs :: [FilePath] -> IO [Hir.Program]
getPrgs hsFiles =
  forM hsFiles $ \file -> do
    fileContents <- Text.readFile file
    let v = parsePrg fileContents
    pure $ snd v

getParPrgs :: [FilePath] -> IO [Hir.Program]
getParPrgs hsFiles = mapConcurrently processFile hsFiles
 where
  processFile file = do
    fileContents <- Text.readFile file -- Read as ByteString (fastest method)
    pure $ snd (parsePrg fileContents) -- Run `test` function

benchmarkMain modIndex debugAllTreeStr prgs =
  defaultMainWith
    (defaultConfig {resamples = 1})
    [ bench "mem" $ nf ((fmap (.name)) . findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR") modIndex
    , bench "rename" $ nf debugAllTreeStr ()
    ]

config :: Config
config =
  defaultConfig
    { resamples = 1
    }

time :: IO a -> [Char] -> IO ()
time fn label = do
  start <- getCurrentTime
  _ <- fn
  end <- getCurrentTime
  putStrLn $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
