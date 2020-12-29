{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Char.Extra (
  bom,
  eolOrEof,
) where

import Relude
import Text.Megaparsec (
  MonadParsec,
  Token,
  Tokens,
  eof,
  single,
 )
import Text.Megaparsec.Char (eol)

eolOrEof :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m String
eolOrEof = eol <|> (eof $> "")

bom :: (MonadParsec e s m, Token s ~ Char) => m Char
bom = single '\65279'
