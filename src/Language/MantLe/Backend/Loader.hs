module Language.MantLe.Backend.Loader where

import Control.Exception (try)
import Data.Monoid (Dual (..))
import Data.String.Interpolate (i)
import Language.MantLe.Parser
import Language.MantLe.Parser.Expr.Types
import Language.MantLe.Parser.Statements.Class
import Language.MantLe.Parser.Statements.Data
  ( Data
  )
import Language.MantLe.Parser.Statements.Declare
import Language.MantLe.Parser.Statements.Import
import Language.MantLe.Types (Token (Identifier))
import System.Environment
import System.FilePath
import Text.Parsec (ParseError)

data Import'Error
  = FSError IOError
  | LexError ParseError
  deriving (Show, Eq)

type Import'Result =
  ([Import'Error], Stmt)

identifier :: Token -> String
identifier (Identifier s) = s

load'module :: Import -> IO Import'Result
load'module Import{mod} = do
  basedir <- getEnv "MANTLE_LIBRARY_BASEDIR"
  let
    module'path =
      foldl' (</>) basedir (map identifier mod) <> ".mt"
    module'name =
      foldl'
        (\l r -> [i|#{l}.#{r}|])
        ""
        (map identifier mod)
  module'content' <-
    try @IOError (readFile module'path)
  case module'content' of
    Left e -> return $ ([FSError e], mempty)
    Right module'content ->
      case parse file module'name $ Source module'content of
        Left e -> return $ ([LexError e], mempty)
        Right module' -> return $ return $ preprocess module'

preprocess :: Stmt -> Stmt
preprocess s@Stmt{..} =
  let declares' =
        getDual $ mconcat $ map (Dual . quantify) classes
   in s
        { declares = declares' <> declares
        }

quantify :: Class -> [Declare]
quantify Class{..} =
  map quantify' functions
 where
  quantify' :: Declare -> Declare
  quantify' d =
    d
      { datatype =
          Type'Forall
            types
            [Constraint name $ map Type'Var types]
            d.datatype
      }

load'module'rec :: Import -> IO Import'Result
load'module'rec i = do
  loaded <- load'module i
  case loaded of
    ([], s) -> do
      let
        imports = s.imports
        loaded's = traverse id $ map load'module imports
      results <- loaded's
      let (es, s) = traverse id $ results
      return $ (es, mconcat s)
    (es, s) -> return $ (es, s)
