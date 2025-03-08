module Main where

import HaskellAnalyzer
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Text.Pretty.Simple
import Control.Monad (forM)
import System.IO
import Criterion.Main
import Criterion.Types (Config(..))
import Arborist.Renamer.GlobalEnv
import qualified Hir.Types as Hir
import UnliftIO (mapConcurrently)
import qualified Data.Text.IO.Utf8 as BS
import qualified Data.Text.Encoding as TE
import Data.Time
import GHC.IO (unsafeInterleaveIO)
import Debug.Trace
import Arborist.Renamer (rename)

main :: IO ()
main = do
  hsFiles <- getHsFiles "../mercury-web-backend/src"
  -- mapM_ putStrLn hsFiles

  prgs <- lazyGetPrgs hsFiles
  let modIndex = mkModIndex prgs
  let found = findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR" modIndex
  --benchmarkMain modIndex prgs

  withFile "myFile.txt" AppendMode $ \h -> do
        hSetEncoding h utf8  -- Set UTF-8 encoding
        hPutStrLn h (Text.unpack (pShowNoColor (head prgs)))
        hPutStrLn h (Text.unpack (pShowNoColor $ rename (head prgs).haskell))

lazyGetPrgs :: [FilePath] -> IO [Hir.Program]
lazyGetPrgs hsFiles = do
    entries <- forM hsFiles $ \file -> do
        prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
        pure (file, prog)
    pure $ snd <$> entries

parseFile :: [Char] -> IO Hir.Program
parseFile file = do
        traceShowM $ "parsing: " <> file
        fileContents <- BS.readFile file  -- Read as ByteString (fastest method)
        pure $ snd (parsePrg fileContents)  -- Run `test` function

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
        fileContents <- BS.readFile file  -- Read as ByteString (fastest method)
        pure $ snd (parsePrg fileContents)  -- Run `test` function

benchmarkMain modIndex prgs = 
  defaultMainWith (defaultConfig { resamples = 1 })
    [
      bench "mem" $ nf ((fmap (.name)) . findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR") modIndex
    ]

config :: Config
config = 
  defaultConfig
    {
      resamples = 1
    }

time :: IO a -> [Char] -> IO ()
time fn label= do
  start <- getCurrentTime
  _ <- fn
  end <- getCurrentTime
  putStrLn $ "Time to run " <> label <>": " ++ show (diffUTCTime end start)
