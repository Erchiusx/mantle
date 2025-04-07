module Main where

import Language.MantLe.Math (evaluate)

main :: IO ()
main = do
  res <- evaluate "Sum[x+1, {x,1,3}]"
  print res
