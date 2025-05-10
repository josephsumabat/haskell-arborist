module A.C (cFunc, Thing(..)) where

cFunc :: String
cFunc = "from C"

data Thing = ThingC | ThingD deriving (Show)
