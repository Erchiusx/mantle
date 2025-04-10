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
  , many1
  , optional
  , try
  , (<|>)
  )

val'expr' :: Parser u Val'Expr
val'expr' =
  choice $
    map try $
      [ p'application
      , p'case'of
      , p'let'in
      , p'type'application
      , p'lambda
      , p'plain
      ]

p'let'in :: Parser u Val'Expr
p'let'in = do
  Keyword Let <- anyToken
  Layout Indent <- anyToken
  bindings <-
    many $ try (binding <* optional parallel)
  Keyword In <- anyToken
  value <- may'indent val'expr
  return $ Val'Let (bindings) value

binding :: Parser u (Pattern, Val'Expr)
binding = do
  p <- pattern'
  Symbol Bind'to <- anyToken
  value <- val'expr
  return (p, value)

p'case'of :: Parser u Val'Expr
p'case'of = do
  Keyword Case <- anyToken
  value <- val'expr
  Keyword Of <- anyToken
  Layout Indent <- anyToken
  branches <- many $ try (branch <* parallel)
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
  value <- p'plain
  Symbol TApp <- anyToken
  type' <- type'expr
  return $ Val'Sig value type'

p'application :: Parser u Val'Expr
p'application = do
  f <- component
  rest <- many1 component
  return $ foldl' Val'App f rest

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

any'operator :: Parser u Token
any'operator = do
  o@(Operator _) <- anyToken
  return o

fcomponent :: Parser u Val'Expr
fcomponent =
  choice $
    map try $
      [ paren'enclosed val'expr
      , p'application
      , p'plain
      ]

val'expr :: Parser u Val'Expr
val'expr =
  ( try $ do
      co <- many1 $ try $ do
        c <- fcomponent
        o <- any'operator
        return (c, o)
      c <- fcomponent
      if length co == 0
        then return c
        else do
          let (c', o) = traverse (\(x, y) -> ([x], y)) co
          return $ Val'Formula o $ c' ++ [c]
  )
    <|> val'expr'
