module Language.MantLe.Parser.Types where

import Control.Monad.State
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
  -> (Either ParseError a, Parser'State)
parse p u name source =
  runState (runParserT p u name source) $
    Parser'State
      { indent = [0]
      , exdent = 0
      }

class Statement a where
  expect :: Parser u a

all'tokens :: Parser u [Token]
all'tokens = many anyToken

may'indent :: Parser u a -> Parser u a
may'indent p = indented p <|> p
 where
  indented :: Parser u a -> Parser u a
  indented p = try $ do
    Layout Indent <- anyToken
    res <- may'indent p
    Layout Exdent <- anyToken
    return res

parallel :: Parser u ()
parallel = try $ do
  Layout Parallel <- anyToken
  return ()
