module Scripts.PrintDeps (PrintDepsOptions (..), runPrintDeps) where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import HaskellAnalyzer (parsePrg)
import Hir.Read.Types qualified as Hir.Read
import Hir.Types qualified as Hir
import System.IO (stderr)

data PrintDepsOptions = PrintDepsOptions
  { sourceFile :: FilePath
  }

runPrintDeps :: PrintDepsOptions -> IO ()
runPrintDeps PrintDepsOptions {sourceFile} = do
  contents <- TE.decodeUtf8 <$> BS.readFile sourceFile
  let (parseErrors, program) = parsePrg contents :: ([T.Text], Hir.Read.Program)
  unless (null parseErrors) $ do
    TIO.hPutStrLn stderr $ "Encountered parse errors while analyzing " <> T.pack sourceFile <> ":"
    for_ parseErrors $ \err -> TIO.hPutStrLn stderr ("  " <> err)
  for_ (map (.mod) program.imports) $ \modName ->
    TIO.putStrLn modName.text
