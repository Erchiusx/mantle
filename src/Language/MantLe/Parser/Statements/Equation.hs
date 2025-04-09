module Language.MantLe.Parser.Statements.Equation (Equation (..)) where

import Control.Monad (guard)
import Data.List.Split (splitOn)
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Expr.Val
import Language.MantLe.Parser.Types
import Language.MantLe.Types
import Text.Parsec (try)

data Equation
  = Equation
  { lhs :: Val'Expr
  , rhs :: Val'Expr
  }
  deriving (Show, Eq)

instance Statement Equation where
  expect = try $ do
    Val'Formula os vs <- val'expr
    guard $ Operator "=" `elem` os
    let parts = os `splitOn` [Operator "="]
    guard $ length parts == 2
    first : second : [] <- return parts
    let
      (left, right) =
        splitAt (length first + 1) vs
      lhs = Val'Formula first left
      rhs = Val'Formula second right
    return $ Equation lhs rhs
