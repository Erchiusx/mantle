module Language.MantLe.Parser.Expr.Type (type'expr) where

import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types
  ( Keyword (..)
  , Paren'Type (..)
  , Symbol (..)
  , Token (..)
  )
import Text.Parsec
  ( anyToken
  , choice
  , many
  , try
  , (<|>)
  )

type'expr :: Parser Type'Expr
type'expr =
  choice $
    map try $
      [ pForall
      , pExists
      , pFn
      , pADT
      , pVar
      ]

type'expr'nofn :: Parser Type'Expr
type'expr'nofn =
  choice $
    map try $
      [ pForall
      , pExists
      , pFn
      , pSimpleType
      ]

pVar :: Parser Type'Expr
pVar = do
  Identifier name <- anyToken
  return $ Type'Var $ Identifier name

pFn :: Parser Type'Expr
pFn = do
  t1 <- pSimpleType
  Symbol Map'to <- anyToken
  t2 <- type'expr
  pure (Type'Fn t1 t2)

pForall :: Parser Type'Expr
pForall = do
  Keyword Forall <- anyToken
  typeVars <- many $ try $ do
    Identifier name <- anyToken
    return $ Identifier name
  constraints <- pConstraints
  Symbol Constraint'symbol <- anyToken -- =>
  typ <- type'expr
  return $ Type'Forall typeVars constraints typ

pExists :: Parser Type'Expr
pExists = do
  Keyword Exists <- anyToken
  typeVars <- many $ try $ do
    Identifier name <- anyToken
    return $ Identifier name
  constraints <- pConstraints
  Symbol Constraint'symbol <- anyToken -- =>
  typ <- type'expr
  return $ Type'Exists typeVars constraints typ

pConstraints :: Parser [Constraint]
pConstraints = many $ try $ do
  Symbol Dot <- anyToken
  Identifier classname <- anyToken
  types <- many type'expr'nofn
  return $
    Constraint (Identifier classname) types

pADT :: Parser Type'Expr
pADT = do
  t1 <- pSimpleType
  t2 <- pSimpleType
  pure (Type'App t1 t2)

pSimpleType :: Parser Type'Expr
pSimpleType = try pVar <|> parens type'expr

parens :: Parser a -> Parser a
parens p = do
  Symbol (Paren Round False) <- anyToken
  x <- p
  Symbol (Paren Round True) <- anyToken
  return x
