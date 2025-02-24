module Main where

import HaskellAnalyzer
import qualified Data.Text.IO.Utf8 as Utf8
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Text.Pretty.Simple
import Control.Monad (forM)
import System.IO

main :: IO ()
main = do
  hsFiles <- getHsFiles "../mercury-web-backend/src"
  -- mapM_ putStrLn hsFiles
  mem <- forM hsFiles $ \file -> do
    fileContents <- Utf8.readFile file
    let v = test fileContents
        content = Text.unpack (pShowNoColor v)
    pure content

  withFile "myFile.txt" AppendMode $ \h -> do
        hSetEncoding h utf8  -- Set UTF-8 encoding
        hPutStrLn h (head mem) -- Append text
