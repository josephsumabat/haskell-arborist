{-# LANGUAGE TemplateHaskell #-}

module PersistentModels.NoImport where

mkModel $(discoverEntities) $(modelFile "should_ignore")
