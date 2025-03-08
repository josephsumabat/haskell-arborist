module Arborist.ModExports where

data NameInfo =
      NameInfo
        {
          name :: T.Text
        , dynNode :: DynNode
        }

data ModExports = [NameInfo]
