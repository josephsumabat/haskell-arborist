module Arborist.DocIndex where

import Arborist.ProgramIndex
import qualified Data.HashMap.Lazy as Map
import Hir.Types
import AST.Haskell

type DocIndex = Map.HashMap ModuleText HaddockP

