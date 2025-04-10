module Language.MantLe.Parser.Statements.Instance where

import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Types qualified as ET
import Language.MantLe.Parser.Statements.Binding qualified as Binding
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, many, sepBy, try)

data Instance
  = Instance
  { typeclass :: T.Token
  , types :: [ET.Type'Expr]
  , functions :: [Binding.Binding]
  }

instance Statement Instance where
  expect = try $ do
    T.Keyword T.Instance <- anyToken
    typeclass@(T.Identifier _) <- anyToken
    types <- many . try $ type'expr
    T.Layout T.Indent <- anyToken
    functions <-
      try (expect @Binding.Binding) `sepBy` parallel
    T.Layout T.Exdent <- anyToken
    return $ Instance typeclass types functions
