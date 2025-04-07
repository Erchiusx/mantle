module Language.MantLe.Parser.Types where

import Control.Monad.State
import Debug.Trace
import Language.MantLe.Lexer
import Language.MantLe.Types
import Text.Parsec hiding (State, parse)

type Parser u a =
  ParsecT Source u (State Parser'State) a

parse
  :: Parser u a
  -> u
  -> SourceName
  -> Source
  -> Either ParseError a
parse p u name source =
  evalState (runParserT p u name source) $
    Parser'State
      { indent = [0]
      , exdent = 0
      }

data AST = forall a. AST'node a => AST'node Period a
type Period = (SourcePos, SourcePos)
class AST'node a
class Statement a where
  expect :: Parser () a

all'tokens :: Parser () [Token]
all'tokens = many anyToken
