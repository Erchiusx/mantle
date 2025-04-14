module Language.MantLe.Backend.Typecheck
where

import Control.Monad.State
import Data.List (find)
import Language.MantLe.Parser (Stmt (..))
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Statements.Binding
import Language.MantLe.Parser.Statements.Declare
  ( Declare (..)
  )
import Language.MantLe.Types

data Use'State
  = Use'State
  { name'state :: Stmt
  , mma'state :: Integer
  , module'name :: String
  , mma'vars :: [Binding]
  }
  deriving (Show, Eq)

type MState = StateT Use'State (Either String)

type'check
  :: Val'Expr -> MState Type'Expr
type'check (Box _) = return $ Type'Var $ Identifier "Math"
type'check (Val'Var a) = do
  Stmt{declares} <- name'state <$> get
  let def'type =
        datatype
          <$> find (\Declare{name = a'} -> a' == a) declares
  case def'type of
    Nothing ->
      lift $
        Left $
          "cannot find type for var " <> show a <> "\n"
    Just t -> return t
type'check (Val'Lam p x) = do
  t <- type'check'pattern p
  u <- type'check x
  return $ Type'Fn t u
type'check (Val'App f x) = do
  t'f <- type'check f
  t'x <- type'check x
  case t'f of
    Type'Fn t u -> undefined
type'check (Val'Sig v t) = do
  t' <- type'check v
  lift $ t' `apply'type` t

type'check'pattern :: Pattern -> MState Type'Expr
type'check'pattern = undefined

apply'type
  :: Type'Expr -> Type'Expr -> Either String Type'Expr
(Type'Forall _ _ _) `apply'type` _ = undefined
_ `apply'type` _ = undefined
