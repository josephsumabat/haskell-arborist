{-# LANGUAGE TemplateHaskell #-}

module PersistentModels.Invoice where

import PersistentModels.Import

mkModel $(discoverEntities) $(modelFile "ar_invoice")
