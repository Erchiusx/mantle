module Language.MantLe.Parser.Expr.Type where

import Text.Parsec (getState)
import Language.MantLe.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Parser.Expr.Types


type'expr :: Parser Parser'State Type'Expr
type'expr = do
  Parser'State{indent} <- getState
  
  return undefined
