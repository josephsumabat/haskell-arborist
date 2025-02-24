module HaskellAnalyzer where

import ModContext
import Hir.Parse qualified as Hir
import Hir.Types qualified as Hir
import qualified AST.Haskell as AST
import qualified AST.Unwrap as AST
import qualified AST.Err as AST
import qualified AST.Node as AST
import qualified AST.Cast as AST


test t = Hir.parseHaskell (AST.parse t)
