module TestImport where

testDataDir :: FilePath
testDataDir = "./test-data/base-data"

allTestLibs :: [FilePath]
allTestLibs =
  [ "./test-data/base-control"
  , testDataDir
  ]
