module Language.MantLe.Parser where

import Language.MantLe.Parser.Statements.Binding ( Binding )
import Language.MantLe.Parser.Statements.Class ( Class )
import Language.MantLe.Parser.Statements.Data ( Data )
import Language.MantLe.Parser.Statements.Declare ( Declare )
import Language.MantLe.Parser.Statements.Equation ( Equation )
import Language.MantLe.Parser.Statements.Import ( Import )
import Language.MantLe.Parser.Statements.Object ( Object )
import Language.MantLe.Parser.Types ( Statement(..), Parser )
import Text.Parsec (choice, many, try)

data Stmt = forall a. Statement a => Stmt a

file :: Parser u [Stmt]
file =
  many $
    choice $
      map try $
        [ Stmt <$> expect @Import
        , Stmt <$> expect @Object
        , Stmt <$> expect @Class
        , Stmt <$> expect @Data
        , Stmt <$> expect @Declare
        , Stmt <$> expect @Binding
        , Stmt <$> expect @Equation
        ]
