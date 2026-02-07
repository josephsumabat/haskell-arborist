module Example () where

foo :: Int -> Int
foo x = x + 1

bar :: Bool -> Bool
bar b = not b

(***) :: Int -> Int -> Int
(***) = (+)