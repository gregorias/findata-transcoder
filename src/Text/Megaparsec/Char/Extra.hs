{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Char.Extra (
  bom,
  eolOrEof,
  anyLineP,
) where

import Data.Text (singleton)
import Relude
import Text.Megaparsec (
  MonadParsec,
  Token,
  Tokens,
  anySingle,
  eof,
  label,
  single,
  someTill_,
 )
import Text.Megaparsec.Char (
  eol,
  newline,
 )

eolOrEof :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m (Tokens s)
eolOrEof = eol <|> (eof $> "")

bom :: (MonadParsec e s m, Token s ~ Char) => m Char
bom = single '\65279'

-- | Parses a single line.
--
-- Consumes at least one character.
--
-- The line ends with a newline or an eof.
anyLineP :: (MonadParsec e s m, Token s ~ Char) => m Text
anyLineP = label "arbitrary line" (newlineP <|> nonEmptyLineP)
 where
  newlineP = singleton <$> newline
  nonEmptyLineP = do
    (line, end) <- someTill_ anySingle (newlineP <|> (eof >> return ""))
    return $ toText line <> end
