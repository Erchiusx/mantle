module Language.MantLe.Parser.Statements.Declare where

import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, choice, try)

data Declare = Declare
  { name :: T.Token
  , datatype :: Type'Expr
  }
  deriving (Show, Eq)

instance Statement Declare where
  expect = try $ do
    name@(T.Identifier _) <- binding'name
    T.Symbol T.Type'Note <- anyToken
    type'expr' <- type'expr
    return $ Declare name type'expr'
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
