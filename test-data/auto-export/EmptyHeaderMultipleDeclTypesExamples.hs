module StaticLS.IDE.SourceEdit where

class MyClass a where
  myMethod :: a -> Bool

newtype MyNewtype = MyNewtype Int

data MyData
  = MyDataConstructor Int String

foo :: Int -> Int
foo x = x + 1

(***) :: Int -> Int -> Int
(***) = (+)
