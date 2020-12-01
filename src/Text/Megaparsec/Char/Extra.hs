{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Char.Extra (eolOrEof) where

import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    Token,
    Tokens,
    eof,
    (<|>),
  )
import Text.Megaparsec.Char (eol, string)

eolOrEof :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m String
eolOrEof = eol <|> (eof *> string "")
