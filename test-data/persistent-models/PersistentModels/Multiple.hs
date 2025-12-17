{-# LANGUAGE TemplateHaskell #-}

module PersistentModels.Multiple where

import PersistentModels.Import

mkModel $(discoverEntities) $(modelFile "foo_model")
mkModel $(discoverEntities) $(testModelFile "bar_model")
