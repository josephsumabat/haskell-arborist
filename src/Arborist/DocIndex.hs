module Arborist.DocIndex where

import AST.Haskell
import Arborist.ProgramIndex
import Data.HashMap.Lazy qualified as Map
import Hir.Types

type DocIndex = Map.HashMap ModuleText HaddockP
