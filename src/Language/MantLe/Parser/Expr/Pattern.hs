module Language.MantLe.Parser.Expr.Pattern (pattern') where

import Data.These
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types
import Text.Parsec
  ( anyToken
  , choice
  , many
  , try
  , (<|>)
  )

pattern' :: Parser Pattern
pattern' =
  choice $
    map try' $
      [ syn
      , branch
      , name
      ]

try' :: Parser a -> Parser a
try' p = enclosed <|> try p
 where
  enclosed = try $ do
    Symbol (Paren Round Open) <- anyToken
    res <- try' p
    Symbol (Paren Round Close) <- anyToken
    return res

name :: Parser Pattern
name = do
  Identifier name' <- anyToken
  return . Pattern . This . Identifier $ name'

branch :: Parser Pattern
branch = do
  Symbol (Paren Round Open) <- anyToken
  Identifier branch'name <- anyToken
  patterns <- many $ try pattern'
  Symbol (Paren Round Close) <- anyToken
  return . Pattern $
    That (Identifier branch'name, patterns)

syn :: Parser Pattern
syn = do
  Pattern (This name') <- name
  Symbol TApp <- anyToken
  Pattern (That branch') <- branch
  return . Pattern $ These name' branch'
