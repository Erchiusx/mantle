module Language.MantLe.Parser.Statements.Import where

import Text.Parsec (anyToken, many, try)
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T

newtype Import = Import {
    mod :: [T.Token]
  }

instance AST'node Import
instance Statement Import where
  expect = try $ do
    T.Keyword T.Import <- anyToken
    first@(T.Identifier _) <- anyToken
    rest <- many $ do
      T.Symbol T.Dot <- anyToken
      id@(T.Identifier _) <- anyToken
      return id
    return $ Import $ first : rest
