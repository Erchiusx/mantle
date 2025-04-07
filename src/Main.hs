module Main where

import Language.MantLe.Lexer (Source (Source))
import Language.MantLe.Parser.Types

main :: IO ()
main = do
  file <- readFile "./test.ml"
  case parse all'tokens () "stdin" $ Source file of
    Left e -> print e
    Right res -> mconcat $ map print res
