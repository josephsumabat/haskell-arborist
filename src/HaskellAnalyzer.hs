module HaskellAnalyzer where

import AST.Haskell qualified as AST
import Hir.Parse qualified as Hir

parsePrg t = Hir.parseHaskell (AST.parse t)
