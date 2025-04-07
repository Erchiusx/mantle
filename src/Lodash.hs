module Lodash where

infixl 1 $|
($|) :: a -> (a -> b) -> b
($|) = flip ($)
