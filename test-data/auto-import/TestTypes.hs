module TestTypes
  ( MyClass(..)
  , MyNewtype(..)
  , MyData(..)
  , foo
  , (***)
  )
where

import Prelude

class MyClass a where
  myMethod :: a -> Bool
  myMethod = undefined 

newtype MyNewtype = MyNewtype Int

data MyData
  = MyDataConstructor Int String

foo :: Int -> Int
foo x = x + 1 


(***) :: Int -> Int -> Int
(***) = (+)

type family MyTypeFamily a :: *
type instance MyTypeFamily Int = Bool

data family MyDataFamily a
data instance MyDataFamily Int = MyDataFamilyInt Int
