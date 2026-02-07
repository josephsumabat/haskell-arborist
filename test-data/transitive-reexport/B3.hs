module B3 (module C, module C2) where

import C hiding (myFunc2)
import C2 hiding (myFunc) 