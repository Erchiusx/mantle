module Language.MantLe.Parser where

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
  )
import Text.Parsec (choice, many)

data Stmt = forall a. Statement a => Stmt a

file :: Parser u [Stmt]
file =
  many $
    choice $
      [ Stmt <$> expect @Import
      , Stmt <$> expect @Object
      , Stmt <$> expect @Class
      , Stmt <$> expect @Instance
      , Stmt <$> expect @Data
      , Stmt <$> expect @Declare
      , Stmt <$> expect @Binding
      , Stmt <$> expect @Equation
      ]
