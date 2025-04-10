module Main where

import Language.MantLe.Lexer (Source (Source))
import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Val
import Language.MantLe.Parser.Types
import Language.MantLe.Types (Token (Identifier))
import Text.Parsec (anyToken, many, try)

main :: IO ()
main = do
  file <- readFile "./test.ml"
  print $ parse p'let'in () "stdin" $ Source file
