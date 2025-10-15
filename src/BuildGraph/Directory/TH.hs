module BuildGraph.Directory.TH (targetKeyTagModifier) where

-- | Map sum constructor names to the JSON tag label expected by our output
targetKeyTagModifier :: String -> String
targetKeyTagModifier constructor =
  case constructor of
    "DirectoryTargetOutput" -> "directory"
    "ModuleTargetOutput" -> "module"
    other -> other
