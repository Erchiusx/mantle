module Language.MantLe.Parser.Expr.Type where

import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types
import Text.Parsec (getState)

type'expr :: Parser () Type'Expr
type'expr = do
  return undefined
