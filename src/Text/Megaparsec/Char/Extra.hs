{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Char.Extra (eolOrEof, space) where

import Text.Megaparsec
  ( MonadParsec,
    Token,
    Tokens,
    eof,
    single,
    (<|>),
  )
import Text.Megaparsec.Char (eol, string)

eolOrEof :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m String
eolOrEof = eol <|> (eof *> string "")

space :: (MonadParsec e s m, Token s ~ Char) => m Char
space = single ' '
