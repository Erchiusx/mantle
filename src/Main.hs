module Main where

import Language.MantLe.Lexer (Source (Source))
import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Types
import Language.MantLe.Types (Token (Identifier))
import Text.Parsec (anyToken, many, try)

main :: IO ()
main = do
  file <- readFile "./test.ml"
  case parse type'expr () "stdin" $ Source file of
    Left e -> print e
    Right res -> print res
