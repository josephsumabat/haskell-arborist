module A.B (bFunc, Thing(..)) where

bFunc :: Int
bFunc = 42

data Thing = ThingA | ThingB deriving (Show)
