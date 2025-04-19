module Main where

import AST.Cast
import AST.Haskell qualified
import AST.Haskell qualified as AST
import AST.Traversal (getDeepestContainingLineCol)
import Arborist.Debug
import Arborist.ModGraph
import Arborist.Renamer (RenamePhase, renamePrg)
import Arborist.Renamer.GlobalEnv
import Arborist.Scope
import Arborist.Scope.Global
import Control.Monad (forM)
import Criterion.Main
import Criterion.Types (Config (..))
import Data.Either
import Data.HashMap.Lazy qualified as Map
import Data.LineCol
import Data.LineColRange
import Data.Maybe (catMaybes, fromJust)
import Data.Pos
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
  time "everything" $ do
    let srcWithLps =
          ( [ "../mercury-web-backend/src"
            , "../mercury-web-backend/local-packages/a-mercury-prelude/src"
            , "../mercury-web-backend/local-packages/bugsnag-mtl/src"
            , "../mercury-web-backend/local-packages/cache-variable/src"
            , "../mercury-web-backend/local-packages/chalk-http/src"
            , "../mercury-web-backend/local-packages/evolve-types/src"
            , "../mercury-web-backend/local-packages/fedwire-reference/src"
            , "../mercury-web-backend/local-packages/fixed-width-parser/src"
            , "../mercury-web-backend/local-packages/fixed-width-text/src"
            , "../mercury-web-backend/local-packages/hspec-flaky/src"
            , "../mercury-web-backend/local-packages/initialize-test-databases/src"
            , "../mercury-web-backend/local-packages/linear-api/src"
            , "../mercury-web-backend/local-packages/mercury-account-number/src"
            , "../mercury-web-backend/local-packages/mercury-ach-codec/src"
            , "../mercury-web-backend/local-packages/mercury-ach-ref/src"
            , "../mercury-web-backend/local-packages/mercury-aeson/src"
            , "../mercury-web-backend/local-packages/mercury-arbitrary/src"
            , "../mercury-web-backend/local-packages/mercury-banking-base/src"
            , "../mercury-web-backend/local-packages/mercury-banking/src"
            , "../mercury-web-backend/local-packages/mercury-benchmark/src"
            , "../mercury-web-backend/local-packages/mercury-casing/src"
            , "../mercury-web-backend/local-packages/mercury-conduit/src"
            , "../mercury-web-backend/local-packages/mercury-country/src"
            , "../mercury-web-backend/local-packages/mercury-decision-engine-core/src"
            , "../mercury-web-backend/local-packages/mercury-display/src"
            , "../mercury-web-backend/local-packages/mercury-dollar/src"
            , "../mercury-web-backend/local-packages/mercury-electronic-account-type/src"
            , "../mercury-web-backend/local-packages/mercury-eventlog/src"
            , "../mercury-web-backend/local-packages/mercury-extra/src"
            , "../mercury-web-backend/local-packages/mercury-firco/src"
            , "../mercury-web-backend/local-packages/mercury-fixed/src"
            , "../mercury-web-backend/local-packages/mercury-generics/src"
            , "../mercury-web-backend/local-packages/mercury-hspec-assertions/src"
            , "../mercury-web-backend/local-packages/mercury-jsonb/src"
            , "../mercury-web-backend/local-packages/mercury-ledger/src"
            , "../mercury-web-backend/local-packages/mercury-list-utils/src"
            , "../mercury-web-backend/local-packages/mercury-moat/src"
            , "../mercury-web-backend/local-packages/mercury-network-logging/src"
            , "../mercury-web-backend/local-packages/mercury-numeric/src"
            , "../mercury-web-backend/local-packages/mercury-partner-bank/src"
            , "../mercury-web-backend/local-packages/mercury-paste/src"
            , "../mercury-web-backend/local-packages/mercury-persistent/src"
            , "../mercury-web-backend/local-packages/mercury-redis/src"
            , "../mercury-web-backend/local-packages/mercury-refined/src"
            , "../mercury-web-backend/local-packages/mercury-risk-alerts/src"
            , "../mercury-web-backend/local-packages/mercury-risk-policy/src"
            , "../mercury-web-backend/local-packages/mercury-routing-number/src"
            , "../mercury-web-backend/local-packages/mercury-settings/src"
            , "../mercury-web-backend/local-packages/mercury-slack-class/src"
            , "../mercury-web-backend/local-packages/mercury-string-variants/src"
            , "../mercury-web-backend/local-packages/mercury-stubs-internal/src"
            , "../mercury-web-backend/local-packages/mercury-swift-code/src"
            , "../mercury-web-backend/local-packages/mercury-text/src"
            , "../mercury-web-backend/local-packages/mercury-time/src"
            , "../mercury-web-backend/local-packages/mercury-tracing/src"
            , "../mercury-web-backend/local-packages/mercury-transaction-amount/src"
            , "../mercury-web-backend/local-packages/mercury-transaction-kind/src"
            , "../mercury-web-backend/local-packages/mercury-treasury/src"
            , "../mercury-web-backend/local-packages/mercury-typescript/src"
            , "../mercury-web-backend/local-packages/mercury-us-states/src"
            , "../mercury-web-backend/local-packages/mercury-uuid/src"
            , "../mercury-web-backend/local-packages/openapi3-th/src"
            , "../mercury-web-backend/local-packages/raccoon/src"
            , "../mercury-web-backend/local-packages/this-or-that/src"
            , "../mercury-web-backend/local-packages/workflow/src"
            , "../mercury-web-backend/local-packages/working-with-local-packages.md/src"
            , "../mercury-web-backend/local-packages/x9-serialization/src"
            ]
          ) ::
            [String]
    let onlySrc = ["../mercury-web-backend/src"]
    let src = srcWithLps

    hsFiles <- getHsFiles (head src)
    -- mapM_ putStrLn hsFiles

    prgs <- lazyGetPrgs hsFiles
    let modIndex = mkModIndex prgs
    let found = findName "Mobile.InternalAuthorization.Handler" "postMobileTokenLoginR" modIndex
    let haskell = fromJust $ cast @AST.Haskell.HaskellP (last prgs).dynNode
    target <- head <$> lazyGetPrgs ["../mercury-web-backend/src/Mobile/Push.hs"]
    (requiredPrograms, exportIdx) <- time "gather" $ gatherScopeDeps target src
    let modText = parseModuleTextFromText "Mobile.AppLink"
    let renameTree = renamePrg requiredPrograms exportIdx target
    let debugTreeStr = fromJust $ (debugTree . (.dynNode)) <$> renameTree
    let loc = point (LineCol (mkPos 87) (mkPos 29))
    let chosenNode = (getDeepestContainingLineCol @(AST.Variable RenamePhase) loc) . (.dynNode) =<< renameTree
    let allRenamed = (\prg -> renamePrg (prgsToMap prgs) Map.empty prg) <$> prgs
    let debugAllTreeStr _s = catMaybes $ fmap (debugTree . (.dynNode)) <$> allRenamed

    -- benchmarkMain modIndex debugAllTreeStr prgs

    -- _ <- putStrLn debugTreeStr

    time "writefiles" $
      ( withFile "myFile.txt" AppendMode $ \h -> do
          hSetEncoding h utf8 -- Set UTF-8 encoding
          -- hPutStrLn h (Text.unpack (pShowNoColor (head prgs)))
          -- hPutStrLn h (Text.unpack $ pShowNoColor (getAvailableNames requiredPrograms (last prgs)))
          hPutStrLn h (debugTreeStr)
          hPutStrLn h (Text.unpack . pShowNoColor $ chosenNode)
      )

lazyGetPrgs :: [FilePath] -> IO [Hir.Program]
lazyGetPrgs hsFiles = do
  entries <- forM hsFiles $ \file -> do
    prog <- unsafeInterleaveIO (parseFile file) -- Lazily read & parse file
    pure (file, prog)
  pure $ snd <$> entries

parseFile :: [Char] -> IO Hir.Program
parseFile file = do
  -- traceShowM $ "parsing: " <> file
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

time :: [Char] -> IO a -> IO a
time label fn = do
  start <- getCurrentTime
  res <- fn
  end <- getCurrentTime
  putStrLn $ "Time to run " <> label <> ": " ++ show (diffUTCTime end start)
  pure res
