module Language.MantLe.Backend.Loader where

import Language.MantLe.Backend.Interpreter
import Language.MantLe.Parser.Statements.Import
import Text.Parsec (ParseError)
import System.Environment
import System.FilePath
import Language.MantLe.Types (Token (Identifier))
import Control.Exception (try)
import Data.String.Interpolate (i)
import Language.MantLe.Parser

data Import'Error
  = FSError IOError
  | LexError ParseError
  deriving (Show, Eq)

type Import'Result
  = ([Import'Error], Stmt)

identifier :: Token -> String
identifier (Identifier s) = s

load'module :: Import -> IO Import'Result
load'module Import{mod} = do
  basedir <- getEnv "MANTLE_LIBRARY_BASEDIR"
  let
    module'path = foldl' (</>) basedir (map identifier mod) <> ".mt"
    module'name = foldl' (\l r->[i|#{l}.#{r}|]) "" (map identifier mod)
  module'content' <- try @IOError (readFile module'path)
  case module'content' of
    Left e -> return $ ([FSError e], mempty)
    Right module'content ->
      case parse file () module'name $ Source module'content of
        (Left e, _) -> return $ ([LexError e], mempty)
        (Right module', _) -> return $ return $ module'

load'module'rec :: Import -> IO Import'Result
load'module'rec i = do
  loaded <- load'module i
  case loaded of
    ([], s) -> do
      let
        imports = s.imports
        loaded's = traverse id $ map load'module imports
        result = traverse id <$> loaded's
      (es, stmts) <- result
      let stmt = mconcat stmts
      return (es, s <> stmt)
    (es, s) -> return $ (es, s)

