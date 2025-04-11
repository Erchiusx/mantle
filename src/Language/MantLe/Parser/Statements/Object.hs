module Language.MantLe.Parser.Statements.Object where

import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, try)

data Object
  = Object
  { name :: T.Token
  }
  deriving (Show, Eq)

instance Statement Object where
  expect = try $ do
    T.Keyword T.Object <- anyToken
    name@(T.Identifier _) <- anyToken
    return $ Object name
