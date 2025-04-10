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

type'expr :: Parser u Type'Expr
type'expr =
  choice $
    map try $
      [ pForall
      , pExists
      , pFn
      , pADT
      , pVar
      ]

type'expr'nofn :: Parser u Type'Expr
type'expr'nofn =
  choice $
    map try $
      [ pForall
      , pExists
      , pFn
      , pSimpleType
      ]

pVar :: Parser u Type'Expr
pVar = do
  Identifier name <- anyToken
  return $ Type'Var $ Identifier name

pFn :: Parser u Type'Expr
pFn = do
  t1 <- pSimpleType
  Symbol Map'to <- anyToken
  t2 <- type'expr
  pure (Type'Fn t1 t2)

pForall :: Parser u Type'Expr
pForall = do
  Keyword Forall <- anyToken
  typeVars <- many $ try $ do
    Identifier name <- anyToken
    return $ Identifier name
  constraints <- pConstraints
  Symbol Constraint'symbol <- anyToken -- =>
  typ <- type'expr
  return $ Type'Forall typeVars constraints typ

pExists :: Parser u Type'Expr
pExists = do
  Keyword Exists <- anyToken
  typeVars <- many $ try $ do
    Identifier name <- anyToken
    return $ Identifier name
  constraints <- pConstraints
  Symbol Constraint'symbol <- anyToken -- =>
  typ <- type'expr
  return $ Type'Exists typeVars constraints typ

pConstraints :: Parser u [Constraint]
pConstraints = many $ try $ do
  Symbol Dot <- anyToken
  Identifier classname <- anyToken
  types <- many type'expr'nofn
  return $
    Constraint (Identifier classname) types

pADT :: Parser u Type'Expr
pADT = do
  t1 <- pSimpleType
  t2 <- pSimpleType
  pure (Type'App t1 t2)

pSimpleType :: Parser u Type'Expr
pSimpleType = try pVar <|> parens type'expr

parens :: Parser u a -> Parser u a
parens p = do
  Symbol (Paren Round False) <- anyToken
  x <- p
  Symbol (Paren Round True) <- anyToken
  return x
