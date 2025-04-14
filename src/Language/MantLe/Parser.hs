module Language.MantLe.Parser
  ( file
  , Stmt (..)
  , parse
  , Source (..)
  )
where

import Control.Monad.State
import Language.MantLe.Lexer (Source (..))
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Statements.Binding
import Language.MantLe.Parser.Statements.Class
import Language.MantLe.Parser.Statements.Data
import Language.MantLe.Parser.Statements.Declare
import Language.MantLe.Parser.Statements.Equation
import Language.MantLe.Parser.Statements.Import
import Language.MantLe.Parser.Statements.Instance
import Language.MantLe.Parser.Statements.Object
import Language.MantLe.Parser.Types
  ( Parser
  , Statement (..)
  , parallel
  , parse
  )
import Language.MantLe.Types
import Text.Parsec (choice, many, optional)

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Semigroup (Max (..))
import Data.String.Interpolate (i)
import Data.These

data Stmt
  = Stmt
  { bindings :: [Binding]
  , classes :: [Class]
  , datatypes :: [Data]
  , declares :: [Declare]
  , equations :: [Equation]
  , instances :: [Instance]
  , imports :: [Import]
  , objects :: [Object]
  }
  deriving Eq

instance Show Stmt where
  show Stmt{..} =
    [i|(Stmt {
        bindings:
          #{show bindings}
        classes:
          #{show classes}
        datatypes:
          #{show datatypes}
        declares:
          #{show declares}
        equations:
          #{show equations}
        instances:
          #{show instances}
        imports:
          #{show imports}
      })
      |]

class ToStmt a where
  toStmt :: a -> Stmt

instance ToStmt Binding where
  toStmt a = mempty{bindings = [a]}

instance ToStmt Class where
  toStmt a = mempty{classes = [a]}

instance ToStmt Data where
  toStmt a = mempty{datatypes = [a]}
instance ToStmt Declare where
  toStmt a = mempty{declares = [a]}
instance ToStmt Equation where
  toStmt a = mempty{equations = [a]}
instance ToStmt Import where
  toStmt a = mempty{imports = [a]}
instance ToStmt Instance where
  toStmt a = mempty{instances = [a]}
instance ToStmt Object where
  toStmt a = mempty{objects = [a]}

instance Semigroup Stmt where
  s <> s' =
    Stmt
      { bindings = bindings s <> bindings s'
      , classes = classes s <> classes s'
      , datatypes = datatypes s <> datatypes s'
      , declares = declares s <> declares s'
      , equations = equations s <> equations s'
      , instances = instances s <> instances s'
      , imports = imports s <> imports s'
      , objects = objects s <> objects s'
      }

instance Monoid Stmt where
  mempty = Stmt [] [] [] [] [] [] [] []

file :: Parser Stmt
file =
  mconcat <$> do
    many $
      choice
        [ toStmt <$> expect @Import
        , toStmt <$> expect @Object
        , toStmt <$> expect @Class
        , toStmt <$> expect @Instance
        , toStmt <$> expect @Data
        , toStmt <$> expect @Declare
        , toStmt <$> expect @Binding
        , toStmt <$> expect @Equation
        ]
        <* optional parallel
