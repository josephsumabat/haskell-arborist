module B (module C, myFunc) where

import C hiding (myFunc)

myFunc = id 