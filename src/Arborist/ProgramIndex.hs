{-# LANGUAGE TupleSections #-}

module Arborist.ProgramIndex (ProgramIndex, prgsToMap) where

import Control.Error
import Data.HashMap.Lazy qualified as Map
import Hir.Types
import Hir.Types qualified as Hir

-- | In memory index of module -> program
type ProgramIndex = Map.HashMap ModuleText Hir.Program

prgsToMap :: [Hir.Program] -> ProgramIndex
prgsToMap prgs =
  Map.fromList $
    mapMaybe
      ( \prg ->
          (,prg) <$> prg.mod
      )
      prgs
