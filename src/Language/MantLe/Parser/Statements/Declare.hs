module Language.MantLe.Parser.Statements.Declare where

import Text.Parsec (anyToken, try)
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Expr.Type

data Declare = Declare {
  name :: String,
  datatype :: Type'Expr
}

instance AST'node Declare
instance Statement Declare where
  expect = try $ do
    T.Identifier name <- anyToken
    T.Symbol T.Type'Note <- anyToken
    type'expr' <- type'expr
    return $ Declare name type'expr'
