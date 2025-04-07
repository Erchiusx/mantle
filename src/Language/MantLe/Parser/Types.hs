module Language.MantLe.Parser.Types where

import Language.MantLe.Lexer
import Text.Parsec hiding (parse)
import Language.MantLe.Types

type Parser u a = Parsec Source u a

parse
  :: Parser u a
  -> u
  -> SourceName
  -> Source
  -> Either ParseError a
parse = runParser

data AST = forall a. AST'node a => AST'node Period a
type Period = (SourcePos, SourcePos)
class AST'node a
class Statement a where
  expect :: Parser Parser'State a
