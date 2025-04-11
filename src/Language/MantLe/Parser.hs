module Language.MantLe.Parser
  ( file
  , Stmt (..)
  , MState
  , create'var
  , eval
  , Use'State (..)
  )
where

import Control.Monad.State
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
  , Statement (..), parallel
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
  deriving (Eq)

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

file :: Parser u Stmt
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
        ] <* optional parallel

data Use'State
  = Use'State
  { name'state :: Stmt
  , mma'state :: Integer
  , module'name :: String
  , mma'vars :: [Binding]
  }
  deriving (Show, Eq)

type MState = State Use'State

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
-- type signal
eval (Val'Sig v _) = eval v
-- let-in
eval (Val'Let [] v) = eval v
eval (Val'Let ((p, x') : bs) v) = do
  x <- eval x'
  t <- get'expr'type x
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

get'expr'type :: Val'Expr -> MState Type'Expr
get'expr'type = undefined

get'branch'type :: Token -> MState Type'Expr
get'branch'type = undefined
