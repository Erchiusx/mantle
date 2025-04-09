module Language.MantLe.Parser.Token where

import Language.MantLe.Parser.Types
import Language.MantLe.Types
import Text.Parsec
  ( SourcePos
  , anyToken
  , getPosition
  )

period
  :: Parser u a -> Parser u ((SourcePos, SourcePos), a)
period p = do
  pos <- getPosition
  res <- p
  pos' <- getPosition
  return ((pos, pos'), res)

dot :: Parser u ()
dot = do
  Symbol Dot <- anyToken
  return ()
