module TestImport where

testDataDir :: FilePath
testDataDir = "test/test-data/base-data"

allTestLibs :: [FilePath]
allTestLibs =
  [
    "test/test-data/base-control"
  , testDataDir
  ]
