module Language.MantLe.Parser.Expr.Types where

import Language.MantLe.Types

data Val'Expr
  = Box Raw'mma -- mma expression
  | Val'Var Token -- Variable, using identifier
  | Val'Lam -- lambda expression
      Token -- Identifier
      Val'Expr -- RHS of lambda
  | Val'App
      Val'Expr -- f
      Val'Expr -- x
  | Val'Op
      Token -- Operator
      Val'Expr -- lhs
      Val'Expr -- rhs
  | Val'Sig
      Val'Expr -- Val
      Type'Expr -- Type

data Type'Expr
  = Type'Var Token -- Type Variable
  | Type'Lam -- type level lambda
      Token -- Identifier
      Type'Expr -- RHS of lambda
  | Type'App
      Type'Expr
      Type'Expr
  | Type'Val -- dependent on values
      Val'Expr -- wrapped value
  | For'All
      Token -- Identifier
      Type'Expr
  | Exists
      Token
      Type'Expr
