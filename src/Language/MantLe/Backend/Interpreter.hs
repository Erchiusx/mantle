module Language.MantLe.Backend.Interpreter
  ( MState
  , create'var
  , eval
  , Use'State (..)
  )
where

import Control.Monad.State
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Semigroup (Max (Max, getMax))
import Data.String.Interpolate (i)
import Data.These
import Language.MantLe.Backend.Typecheck
import Language.MantLe.Parser
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Statements.Binding
import Language.MantLe.Types

locate :: Token -> [Binding] -> Maybe Binding
locate _ [] = Nothing
locate a (b@(Binding{name}) : rest)
  | a == name = Just b
  | otherwise = locate a rest

create'var :: Val'Expr -> MState Val'Expr
create'var (Val'Var a) = do
  origin@(Use'State{..}) <- get
  let
    mma'state' = mma'state + 1
    val = Box [i|mantle$#{module'name}$#{mma'state}|]
    bind = Binding a val
  put
    origin
      { mma'state = mma'state'
      , mma'vars = bind : mma'vars
      , name'state =
          name'state{bindings = bind : name'state.bindings}
      }
  return val
create'var a = error [i|cannot create var for #{a}|]

eval :: Val'Expr -> MState Val'Expr
eval (Box a) = return $ Box a
eval (Val'Var a) = do
  Stmt{bindings} <- name'state <$> get
  let next = locate a bindings
  case next of
    Just n -> eval n.value
    Nothing -> create'var (Val'Var a)
eval a@(Val'Lam _ _) = return a
-- application
eval (Val'App (Box f) (Box x)) =
  return $ Box [i|(#{f})[#{x}]|]
eval (Val'App (Box f) x) = do
  y <- eval x
  case y of
    Box u -> eval (Val'App (Box f) (Box u))
    _ -> return (Val'App (Box f) x)
eval (Val'App f x) = do
  f' <- eval f
  case f' of
    Box f -> eval (Val'App (Box f) x)
    Val'Lam p v ->
      eval (Val'Let [(p, x)] v)
    _ -> return (Val'App f x)
-- formola
eval (Val'Formula [] v) = eval $ head v
eval (Val'Formula os vs) = do
  let
    fixities = map fixty os
    max'fixty = getMax $ mconcat $ map Max fixities
    position = fromJust $ findIndex (== max'fixty) fixities
    (front, center : rail) = splitAt position os
    (front', lhs : rhs : rail') = splitAt position vs
  eval
    ( Val'Formula
        (front ++ rail)
        ( front'
            ++ (Val'App (Val'App (Val'Var center) lhs) rhs)
            : rail'
        )
    )
-- TODO: deal with types
-- type signal
eval (Val'Sig v _) = eval v
-- let-in
eval (Val'Let [] v) = eval v
eval (Val'Let ((p, x') : bs) v) = do
  x <- eval x'
  t <- type'check x
  case p of
    Pattern (This name@(Identifier _)) -> do
      modify $ bindname name x
      eval $ Val'Let bs v
    Pattern (That (name, ps)) -> do
      t' <- get'branch'type name
      case t' == t of
        True -> undefined
        False -> error ""
    Pattern (These name (branch, ps)) -> do
      modify $ bindname name x
      eval
        ( Val'Let ((Pattern (That (branch, ps)), x') : bs) v
        )
    Pattern _ -> undefined
-- case-of
eval (Val'Match v []) = undefined
eval (Val'Match v ((p, v') : rest)) = undefined

fixty :: Token -> Int
fixty (Operator (s : _)) =
  fromJust $
    findIndex
      (s `elem`)
      [ "+-" :: String
      , "*/"
      , "^!"
      , "$%"
      , "&="
      , "<>"
      ]

bindname
  :: Token -> Val'Expr -> Use'State -> Use'State
bindname n v s =
  s
    { name'state =
        s.name'state
          { bindings = Binding n v : s.name'state.bindings
          }
    }

get'branch'type
  :: Token -> MState Type'Expr
get'branch'type = undefined
