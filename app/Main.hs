module Main where

import AST.Cast
import AST.Haskell qualified
import Arborist.Debug
import Arborist.Renamer (renamePrg)
import Arborist.Renamer.GlobalEnv
import Arborist.Scope
import Control.Monad (forM)
import Criterion.Main
import Criterion.Types (Config (..))
import Data.Maybe (fromJust)
import Data.Text.Encoding qualified as TE
import Data.Text.IO.Utf8 qualified as BS
import Data.Text.IO.Utf8 qualified as Utf8
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import Data.Time
import Debug.Trace
import GHC.IO (unsafeInterleaveIO)
import HaskellAnalyzer
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
  -- benchmarkMain modIndex prgs

  let haskell = fromJust $ cast @AST.Haskell.HaskellP (last prgs).dynNode
  let debugTreeStr = fromJust $ (debugTree . (.dynNode)) <$> renamePrg (last prgs)
  requiredPrograms <- getRequiredScopePrograms (last prgs) [src]
  _ <- putStrLn debugTreeStr
  time
    ( withFile "myFile.txt" AppendMode $ \h -> do
        hSetEncoding h utf8 -- Set UTF-8 encoding
        -- hPutStrLn h (Text.unpack (pShowNoColor (head prgs)))
        -- hPutStrLn h debugTreeStr
        hPutStrLn h (Text.unpack $ pShowNoColor (getAvailableNames requiredPrograms (last prgs)))
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
  fileContents <- BS.readFile file -- Read as ByteString (fastest method)
  pure $ snd (parsePrg fileContents) -- Run `test` function

getPrgs :: [FilePath] -> IO [Hir.Program]
getPrgs hsFiles =
  forM hsFiles $ \file -> do
    fileContents <- Utf8.readFile file
    let v = parsePrg fileContents
    pure $ snd v

getParPrgs :: [FilePath] -> IO [Hir.Program]
getParPrgs hsFiles = mapConcurrently processFile hsFiles
 where
  processFile file = do
    fileContents <- BS.readFile file -- Read as ByteString (fastest method)
    pure $ snd (parsePrg fileContents) -- Run `test` function

benchmarkMain modIndex prgs =
  defaultMainWith
    (defaultConfig {resamples = 1})
    [ bench "mem" $ nf ((fmap (.name)) . findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR") modIndex
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
