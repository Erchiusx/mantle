module Language.MantLe.Parser.Types
  ( Parser
  , parse
  , Statement (..)
  , all'tokens
  , may'indent
  , parallel
  , parse'
  )
where

import Control.Monad.State
import Language.MantLe.Lexer
import Language.MantLe.Types
import Text.Parsec hiding (State, parse)

type Parser' u a =
  ParsecT Source u (State Parser'State) a

type Parser a =
  Parsec [Token] () a

parse'
  :: Parser' u a
  -> u
  -> SourceName
  -> Source
  -> (Either ParseError a, Parser'State)
parse' p u name source =
  runState (runParserT p u name source) $
    Parser'State
      { indent = [0]
      , exdent = 0
      }

parse
  :: Parser a
  -> SourceName
  -> Source
  -> Either ParseError a
parse p name source =
  case parse' all'tokens () name source of
    (Left e, _) -> Left e
    (Right tokens, Parser'State{indent, exdent}) -> do
      runParser p () name $
        filter (not . is'comment) $
          tokens
            ++ take
              (length indent - 1 + exdent)
              (repeat $ Layout Exdent)

class Statement a where
  expect :: Parser a

all'tokens :: Parser' u [Token]
all'tokens = many anyToken

may'indent :: Parser a -> Parser a
may'indent p = indented p <|> p
 where
  indented :: Parser a -> Parser a
  indented p = try $ do
    Layout Indent <- anyToken
    res <- may'indent p
    Layout Exdent <- anyToken
    return res

parallel :: Parser ()
parallel = try $ do
  Layout Parallel <- anyToken
  return ()
