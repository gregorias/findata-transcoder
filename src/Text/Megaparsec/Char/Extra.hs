module Text.Megaparsec.Char.Extra (eolOrEof) where

import Text.Megaparsec
  ( Parsec,
    eof,
    (<|>),
  )
import Text.Megaparsec.Char (eol, string)

eolOrEof :: Parsec () String String
eolOrEof = eol <|> (eof *> string "")
