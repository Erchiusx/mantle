module Main where

import Language.MantLe.Lexer (Source (Source))
import Language.MantLe.Parser
import Language.MantLe.Parser.Expr.Type
import Language.MantLe.Parser.Expr.Val
import Language.MantLe.Parser.Types
import Language.MantLe.Types (Token (Identifier))
import Text.Parsec (anyToken, many, try)
import Control.Monad.State (runState, evalState)
import Data.String.Interpolate (i)
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Math (evaluate)

main :: IO ()
main = do
  source <- readFile "./test.mt"
  -- print $ parse (file) () "stdin" $ Source source
  case parse (file) () "stdin" $ Source source of
    (Left _, _) -> error ""
    (Right s, _) -> do
      let
        (Box mma'expr, _) = 
          (`runState`
            Use'State
              { name'state = s
              , mma'state = 0
              , module'name = "repl"
              , mma'vars = []
              }
          ) $
          eval $ 
          case parse
            (val'expr)
            ()
            "repl"
            "x + '1'" of
            (Left e, _) -> error $ show e
            (Right expr, _) -> expr
      res <- evaluate mma'expr
      print res

