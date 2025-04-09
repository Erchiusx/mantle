module Language.MantLe.Parser.Statements.Declare where

import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, try)

data Declare = Declare
  { name :: String
  , datatype :: Type'Expr
  }
  deriving (Show, Eq)

instance Statement Declare where
  expect = try $ do
    T.Identifier name <- anyToken
    T.Symbol T.Type'Note <- anyToken
    type'expr' <- type'expr
    return $ Declare name type'expr'
