module Language.MantLe.Parser.Statements.Object where

import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken)

data Object
  = Object
  { name :: T.Token
  }

instance Statement Object where
  expect = do
    T.Keyword T.Object <- anyToken
    name@(T.Identifier _) <- anyToken
    return $ Object name
