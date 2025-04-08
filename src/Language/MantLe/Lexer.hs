module Language.MantLe.Lexer
  ( Source (Source)
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Control.Monad.State
import Data.Char (isLetter)
import Data.List (isPrefixOf)
import Language.MantLe.Types
import Text.Parsec (Stream (uncons))

try'prefix
  :: [(String, a)]
  -> String
  -> (a -> Token)
  -> Maybe (Token, Source)
try'prefix [] _ _ = Nothing
try'prefix ((s, k) : rest) src f
  | s `isPrefixOf` src =
      Just (f k, Source $ drop (length s) $ src)
  | otherwise = try'prefix rest src f

newtype Source = Source
  { text :: String
  }

lex'keyword :: String -> Maybe (Token, Source)
lex'keyword s = do
  let keywords =
        [ "import"
        , "let"
        , "in"
        , "object"
        , "class"
        , "forall"
        , "exists"
        , "case"
        , "of"
        , "if"
        , "else"
        , "then"
        , "data"
        ]
          :: [(String, Keyword)]
  try'prefix keywords s Keyword

lex'symbol :: String -> Maybe (Token, Source)
lex'symbol s = do
  let symbols =
        [ "::"
        , "<-"
        , "=>"
        , "("
        , ")"
        , "["
        , "]"
        , "{"
        , "}"
        , "."
        , "|"
        , "->"
        , "\\"
        , "@"
        ]
          :: [(String, Symbol)]
  try'prefix symbols s Symbol

lex'operator :: String -> Maybe (Token, Source)
lex'operator s = do
  let
    operator'components = "!$%^&*<>/+-=" :: [Char]
    (result, rest) = break (not . (`elem` operator'components)) s
  guard (result /= "")
  return $ (Operator result, Source rest)

lex'identifier :: String -> Maybe (Token, Source)
lex'identifier s = do
  let (identifier, rest) =
        break (\c -> not (isLetter c) && not (c == '_')) s
  guard (identifier /= "")
  return (Identifier identifier, Source rest)

lex'comment :: String -> Maybe (Token, Source)
lex'comment s = do
  '-' : '-' : rest <- Just s
  let (comment, rest') = break (== '\n') rest
  return $
    (Comment comment, Source $ rest')

lex'math :: String -> Maybe (Token, Source)
lex'math s = do
  '#' : '@' : rest <- Just s
  let (formula, rest') = continue ("", rest)
  return $ (Digital formula, Source rest')
 where
  continue :: (String, String) -> (String, String)
  continue (consumed, '@' : '#' : rest) = (consumed, rest)
  continue (consumed, '@' : '\n' : rest) = (consumed, '\n' : rest)
  continue (consumed, '\n' : rest) = (consumed, '\n' : rest)
  continue (consumed, []) = (consumed, [])
  continue (consumed, c : cs) = continue (consumed ++ [c], cs)

spaces :: String
spaces = " \v"

count :: String -> Char -> (Int, String)
count [] _ = (0, [])
count s@(c : cs) c'
  | c' /= c = (0, s)
  | otherwise =
      let (cnt, s) = cs `count` c
       in (1 + cnt, s)

instance Stream Source (State Parser'State) Token where
  uncons Source{text = text'} = do
    -- unemitted exdents
    p@Parser'State{exdent} <- get
    if exdent > 0
      then do
        put p{exdent = exdent - 1}
        return $ Just (Layout Exdent, Source text')
      else do
        -- empty line
        let text = dropWhile (`elem` spaces) text'
        case text of
          [] -> return Nothing
          '\n' : text' -> do
            -- layout
            let (line, rest) = break (== '\n') text'
            if (`elem` (" \v\t" :: String)) `all` line
              then uncons $ Source rest -- a completely empty line
              else do
                let (tabs, rest) = text' `count` '\t'
                p@Parser'State{indent} <- get
                let prev = head indent
                case tabs `compare` prev of
                  LT -> do
                    let (dropped, indents) = break (<= tabs) indent
                    put
                      p{indent = indents, exdent = length dropped - 1}
                    return $ Just (Layout Exdent, Source rest)
                  EQ -> return $ Just (Layout Parallel, Source rest)
                  GT -> do
                    put p{indent = tabs : indent}
                    return $ Just (Layout Indent, Source rest)
          _ ->
            return $
              fail ""
                <|> lex'keyword text
                <|> lex'comment text
                <|> lex'symbol text
                <|> lex'identifier text
                <|> lex'math text
                <|> lex'operator text
