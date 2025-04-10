module Language.MantLe.Parser.Statements.Binding where

import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Expr.Val
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, choice, try)

data Binding = Binding
  { name :: T.Token
  , datatype :: Val'Expr
  }
  deriving (Show, Eq)

instance Statement Binding where
  expect = try $ do
    name <- binding'name
    T.Symbol T.Bind'to <- anyToken
    val'expr' <- val'expr
    return $ Binding name val'expr'
   where
    binding'name :: Parser u T.Token
    binding'name =
      choice $
        map try $
          [ do
              name@(T.Identifier _) <- anyToken
              return name
          , do
              T.Symbol (T.Paren T.Bracket T.Open) <- anyToken
              op@(T.Operator _) <- anyToken
              T.Symbol (T.Paren T.Bracket T.Close) <- anyToken
              return op
          ]
