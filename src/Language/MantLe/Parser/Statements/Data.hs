module Language.MantLe.Parser.Statements.Data where

import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types qualified as T
import Text.Parsec (anyToken, sepBy, try)

data Data
  = Data
  { name :: T.Token
  , branch :: [Branch]
  }
  deriving (Show, Eq)

data Branch
  = Branch
  { name :: T.Token
  , types :: [Type'Expr]
  }
  deriving (Show, Eq)

instance Statement Data where
  expect = try $ do
    T.Keyword T.Data <- anyToken
    name@(T.Identifier _) <- anyToken
    T.Layout T.Indent <- anyToken
    branch <- try (expect @Branch) `sepBy` parallel
    T.Layout T.Exdent <- anyToken
    return $ Data name branch

instance Statement Branch where
  expect = try $ do
    name@(T.Identifier _) <- anyToken
    T.Keyword T.Of <- anyToken
    types <-
      try type'expr
        `sepBy` ( try $ do
                    T.Symbol T.Dot <- anyToken
                    return ()
                )
    return $ Branch name types
