module A
  ( module A.B
  , cFunc
  , name1, name2
  ) where

import A.B
import qualified A.C as C
import qualified A.Conflict1 as C1
import qualified A.Conflict2 as C2
import UtilsExtra (double)

useUtil = double

cFunc :: String
cFunc = C.cFunc

name1 :: String
name1 = C1.name

name2 :: String
name2 = C2.name
