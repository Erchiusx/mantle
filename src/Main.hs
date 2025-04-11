module Main where

-- import Control.Monad.State (evalState, runState)
-- import Data.String.Interpolate (i)
-- import Language.MantLe.Backend.Interpreter
-- import Language.MantLe.Lexer (Source (Source))
-- import Language.MantLe.Math (evaluate)
-- import Language.MantLe.Parser
-- import Language.MantLe.Parser.Expr.Type
-- import Language.MantLe.Parser.Expr.Types
-- import Language.MantLe.Parser.Expr.Val
-- import Language.MantLe.Parser.Types
import Language.MantLe.Types (Token (Identifier))

-- import System.Environment
-- import Text.Parsec (anyToken, many, try)
import Language.MantLe.Backend.Loader
import Language.MantLe.Parser.Statements.Import

main :: IO ()
main = do
  loaded <-
    load'module'rec $ Import [Identifier "core"]
  print loaded
