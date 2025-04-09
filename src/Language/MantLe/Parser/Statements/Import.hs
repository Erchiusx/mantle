module Language.MantLe.Parser.Statements.Import where

import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, many, try)

newtype Import = Import
  { mod :: [T.Token]
  }
  deriving (Show, Eq)

instance Statement Import where
  expect = try $ do
    T.Keyword T.Import <- anyToken
    first@(T.Identifier _) <- anyToken
    rest <- many . try $ do
      T.Symbol T.Dot <- anyToken
      id@(T.Identifier _) <- anyToken
      return id
    return $ Import $ first : rest
