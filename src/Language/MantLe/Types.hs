module Language.MantLe.Types
  ( Token (..)
  , Keyword (..)
  , Symbol (..)
  , Paren'Type (..)
  , Raw'mma
  , Parser'State (..)
  , Layout (..)
  )
where

import Data.Char (toUpper)
import Data.String (IsString (fromString))

data Token
  = Keyword Keyword
  | Digital Raw'mma
  | Identifier String
  | Operator String
  | Comment String
  | Symbol Symbol
  | Layout Layout
  deriving (Eq, Show)

type Raw'mma = String

data Keyword
  = Import
  | Let
  | In
  | Object
  | Class
  | Forall
  | Exists
  | Case
  | Of
  | If
  | Else
  | Then
  | Data
  deriving (Eq, Show, Read)

instance IsString (String, Keyword) where
  fromString s = (s,) $ read $ first'to'upper s
   where
    first'to'upper (s : ss) = toUpper s : ss
    first'to'upper _ = undefined

data Symbol
  = Type'Note
  | Bind'to
  | Constraint'symbol
  | Dot
  | Paren Paren'Type Bool
  | Branch
  | Map'to
  | Lambda
  | TApp
  deriving (Eq, Show)

data Paren'Type
  = Round
  | Bracket
  | Brace
  deriving (Eq, Show)

instance IsString Symbol where
  fromString "::" = Type'Note
  fromString "<-" = Bind'to
  fromString "=>" = Constraint'symbol
  fromString "(" = Paren Round False
  fromString ")" = Paren Round True
  fromString "[" = Paren Bracket False
  fromString "]" = Paren Bracket True
  fromString "{" = Paren Brace False
  fromString "}" = Paren Brace True
  fromString "." = Dot
  fromString "|" = Branch
  fromString "->" = Map'to
  fromString "\\" = Lambda
  fromString "@" = TApp
  fromString _ = undefined

instance IsString (String, Symbol) where
  fromString s = (s, fromString s)

-- layout
data Layout
  = Indent
  | Exdent
  | Parrallel
  deriving (Show, Eq)

-- state
data Parser'State
  = Parser'State
  { indent :: [Int]
  , exdent :: Int
  }
  deriving (Show, Eq)
