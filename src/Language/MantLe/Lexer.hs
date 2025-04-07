module Language.MantLe.Lexer
  ( Source (Source)
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isLetter)
import Data.List (isPrefixOf)
import Language.MantLe.Types
import Text.Parsec (Stream (uncons))

try'prefix
  :: [(String, a)]
  -> String
  -> (a -> Token)
  -> Maybe (Token, String)
try'prefix [] _ _ = Nothing
try'prefix ((s, k) : rest) src f
  | s `isPrefixOf` src = Just (f k, drop (length s) $ src)
  | otherwise = try'prefix rest src f

newtype Source = Source
  { text :: String
  }

lex'indent :: String -> Maybe (Token, String)
lex'indent s = do
  let
    ss = dropWhile (== '\n') s
    (tabs, rest) = break (/= '\t') ss
    num = length tabs
  return (Indent num, rest)

instance Monad m => Stream Source m Token where
  uncons = return . uncons' . trim
   where
    trim :: Source -> String
    trim =
      let spaces = " \v" :: String
       in dropWhile (`elem` spaces) . text

    uncons' :: String -> Maybe (Token, Source)
    uncons' s =
      (fmap . fmap) Source $
        lex'comment s
          <|> lex'indent s
          <|> lex'symbol s
          <|> lex'operator s
          <|> lex'math s
          <|> lex'keyword s
          <|> lex'identifier s

lex'keyword :: String -> Maybe (Token, String)
lex'keyword s = do
  let keywords =
        [ "import"
        , "where"
        , "object"
        , "class"
        , "forall"
        , "exists"
        ]
          :: [(String, Keyword)]
  try'prefix keywords s Keyword

lex'symbol :: String -> Maybe (Token, String)
lex'symbol s = do
  let symbols =
        [ "@"
        , "<-"
        , "=>"
        , "("
        , ")"
        , "["
        , "]"
        , "{"
        , "}"
        , "#"
        ]
          :: [(String, Symbol)]
  try'prefix symbols s Symbol

lex'operator :: String -> Maybe (Token, String)
lex'operator s = do
  let
    operator'components = "!$%^&*<>/\\:" :: [Char]
    (result, rest) = break (not . (`elem` operator'components)) s
  return $ (Operator result, rest)

lex'identifier :: String -> Maybe (Token, String)
lex'identifier s =
  let (identifier, rest) = break (not . isLetter) s
   in if length identifier == 0
        then Nothing
        else Just (Identifier identifier, rest)

lex'comment :: String -> Maybe (Token, String)
lex'comment s = do
  '-' : '-' : rest <- Just s
  let (comment, rest') = break (== '\n') rest
  return $ (Comment comment, dropWhile (== '\n') rest')

lex'math :: String -> Maybe (Token, String)
lex'math s = do
  '#' : rest <- Just s
  let (digit, rest') = break stopper rest
  return $ (Digital digit, rest')
  where
    stopper :: Char -> Bool
    stopper c = elem @[] @Char c "#\n"
