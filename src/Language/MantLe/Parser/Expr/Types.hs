module Language.MantLe.Parser.Expr.Types where

import Control.Monad (guard)
import Data.These
import Language.MantLe.Parser.Types (Parser)
import Language.MantLe.Types
  ( Paren'Type
  , Raw'mma
  , Symbol (Paren)
  , Token (Symbol)
  )
import Text.Parsec (anyToken, (<?>))

data Val'Expr
  = Box Raw'mma -- mma expression
  | Val'Var Token -- Variable, using identifier
  | Val'Lam -- lambda expression
      Pattern -- Identifier
      Val'Expr -- RHS of lambda
  | Val'App
      Val'Expr -- f
      Val'Expr -- x
  | Val'Formula
      [Token] -- operators
      [Val'Expr] -- values
      -- the length of the second param
      -- should be one more than the first
  | Val'Sig
      Val'Expr -- Val
      Type'Expr -- Type
  | Val'Let
      [(Pattern, Val'Expr)]
      Val'Expr
  | Val'Match
      Val'Expr
      [(Pattern, Val'Expr)]
  deriving (Show, Eq)

data Type'Expr
  = Type'Var Token -- Type Variable
  | Type'Math
  | Type'Fn
      Type'Expr
      Type'Expr
  | Type'Forall
      [Token] -- introduced type variables
      [Constraint]
      Type'Expr
  | Type'Exists
      [Token] -- introduced type variables
      [Constraint]
      Type'Expr
  | Type'App
      Type'Expr
      Type'Expr
  deriving (Show, Eq)

data Pattern
  = Pattern
      ( These
          Token -- alias
          (Token, [Pattern]) -- Branch name and args
      )
  deriving (Show, Eq)

data Constraint
  = Constraint
      Class
      [Type'Expr]
  deriving (Show, Eq)

data Class = Class Token
  deriving (Show, Eq)

newtype Kind = Kind Int

paren'enclosed :: Parser u a -> Parser u a
paren'enclosed p = do
  Symbol (Paren paren'type False) <- anyToken
  res <- p
  Symbol (Paren paren'type' True) <- anyToken
  guard (paren'type == paren'type')
    <?> "unmatched parens"
  return res

enclosed :: Paren'Type -> Parser u a -> Parser u a
t `enclosed` p = do
  Symbol (Paren t' False) <- anyToken
  guard (t' == t)
  res <- p
  Symbol (Paren t' True) <- anyToken
  guard (t' == t)
  return res
