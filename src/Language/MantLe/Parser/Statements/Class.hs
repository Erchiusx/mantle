module Language.MantLe.Parser.Statements.Class where

import Language.MantLe.Parser.Statements.Declare
import Language.MantLe.Parser.Types
  ( Statement (expect)
  , parallel
  )
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, many, sepBy, try)

data Class
  = Class
  { types :: [T.Token] -- type identifiers
  , name :: T.Token
  , functions :: [Declare] -- accompanied functions
  }

instance Statement Class where
  expect = try $ do
    T.Keyword T.Class <- anyToken
    name@(T.Identifier _) <- anyToken
    types <- many . try $ do
      T.Identifier name <- anyToken
      return $ T.Identifier name
    T.Layout T.Indent <- anyToken
    functions <-
      try (expect @Declare) `sepBy` parallel
    T.Layout T.Exdent <- anyToken
    return $ Class types name functions
