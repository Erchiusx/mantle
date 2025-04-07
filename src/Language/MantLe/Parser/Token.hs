module Language.MantLe.Parser.Token where

import Text.Parsec ( anyToken, getPosition, getState, putState )
import Language.MantLe.Types
import Language.MantLe.Parser.Types

period :: Parser u a -> Parser u (Period, a)
period p = do
  pos <- getPosition
  res <- p
  pos' <- getPosition
  return ((pos, pos'), res)

dot :: Parser u ()
dot = do
  Symbol Dot <- anyToken
  return ()

linefeed'continue :: Parser Parser'State ()
linefeed'continue = do
  Indent n <- anyToken
  p@Parser'State{indent = indent:rest} <- getState
  if n > indent
    then do
      putState p{indent = n:p.indent}
      return ()
    else fail "exdented"


linefeed'break :: Parser Parser'State ()
linefeed'break = do
  Indent n <- anyToken
  p@Parser'State{indent = indent:rest} <- getState
  if n < indent
    then do
      putState p{indent = rest}
      return ()
    else fail "not exdented"