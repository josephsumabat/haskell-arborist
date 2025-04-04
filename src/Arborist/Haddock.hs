module Arborist.Haddock where

import Data.HashMap.Lazy qualified as Map
import Data.Text qualified as T
import Hir.Types qualified as Hir

data HaddockInfo

indexHaddocks :: Hir.Program -> Map.HashMap T.Text HaddockInfo
indexHaddocks prg = undefined
