module Language.MantLe.Parser.Statements.Class where

import Language.MantLe.Parser.Statements.Declare
import Language.MantLe.Parser.Statements.Equation qualified as Equation
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
  , relations :: [Equation.Equation]
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
    T.Layout layout <- anyToken
    case layout of
      T.Exdent -> return $ Class types name functions []
      T.Parallel -> do
        relations <-
          try (expect @Equation.Equation) `sepBy` parallel
        return $ Class types name functions relations
      T.Indent -> fail "indentation"
