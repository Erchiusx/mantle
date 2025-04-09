module Language.MantLe.Parser.Statements.Binding where

import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Expr.Val
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, try)

data Binding = Binding
  { name :: String
  , datatype :: Val'Expr
  }
  deriving (Show, Eq)

instance Statement Binding where
  expect = try $ may'indent $ do
    T.Identifier name <- anyToken
    T.Symbol T.Bind'to <- anyToken
    val'expr' <- val'expr
    return $ Binding name val'expr'
