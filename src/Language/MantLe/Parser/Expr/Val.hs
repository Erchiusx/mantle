module Language.MantLe.Parser.Expr.Val where

import Language.MantLe.Parser.Expr.Pattern
  ( pattern'
  )
import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Types
import Language.MantLe.Types
import Text.Parsec
  ( anyToken
  , choice
  , many
  , sepBy
  , try
  )

val'expr :: Parser u Val'Expr
val'expr =
  choice $
    map try $
      [ p'formula
      , p'case'of
      , p'let'in
      , p'type'application
      , p'lambda
      , p'application
      , p'plain
      ]

p'let'in :: Parser u Val'Expr
p'let'in = do
  Keyword Let <- anyToken
  Layout Indent <- anyToken
  bindings <- try binding `sepBy` try parallel
  Layout Exdent <- anyToken
  value <- may'indent val'expr
  return $ Val'Let bindings value

binding :: Parser u (Pattern, Val'Expr)
binding = do
  p <- pattern'
  Operator "=" <- anyToken
  value <- val'expr
  return (p, value)

p'case'of :: Parser u Val'Expr
p'case'of = do
  Keyword Case <- anyToken
  value <- val'expr
  Keyword Of <- anyToken
  Layout Indent <- anyToken
  branches <- try branch `sepBy` try parallel
  Layout Exdent <- anyToken
  return $ Val'Match value branches

branch :: Parser u (Pattern, Val'Expr)
branch = do
  p <- pattern'
  Symbol Map'to <- anyToken
  value <- val'expr
  return (p, value)

p'lambda :: Parser u Val'Expr
p'lambda = do
  Symbol Lambda <- anyToken
  p <- pattern'
  Symbol Dot <- anyToken
  value <- val'expr
  return $ Val'Lam p value

p'type'application :: Parser u Val'Expr
p'type'application = do
  value <- val'expr
  Symbol TApp <- anyToken
  type' <- type'expr
  return $ Val'Sig value type'

p'application :: Parser u Val'Expr
p'application = do
  f <- component
  x <- val'expr
  return $ Val'App f x

component :: Parser u Val'Expr
component =
  choice $
    map try $
      [ paren'enclosed val'expr
      , p'plain
      ]

p'plain :: Parser u Val'Expr
p'plain = do
  t <- anyToken
  case t of
    Digital m -> return $ Box m
    Identifier _ -> return $ Val'Var t
    _ -> fail "error parsing expression"

p'formula :: Parser u Val'Expr
p'formula = do
  co <- many $ try $ do
    c <- component
    o <- any'operator
    return (c, o)
  c <- component
  let (c', o) = traverse (\(x, y) -> ([x], y)) co
  return $ Val'Formula o $ c' ++ [c]

any'operator :: Parser u Token
any'operator = do
  o@(Operator _) <- anyToken
  return o
