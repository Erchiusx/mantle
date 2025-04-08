module Language.MantLe.Backend.MMA.Types where

class ToMMA a where
  to'mma'expr :: a -> String
