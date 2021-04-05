{-# LANGUAGE OverloadedStrings #-}

-- | Module implementing a smart ISIN type.
module Hledupt.Data.Isin (
  Isin,
  unIsin,
  mkIsin,
  isinP,
) where

import Control.Applicative.Combinators (count)
import Relude
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, letterChar, numberChar)

-- | A type representing an ISIN
newtype Isin = Isin
  { unIsin :: Text
  }
  deriving newtype (Eq, Ord, Show)

-- | The ISIN parser
isinP :: MP.Parsec Void Text Isin
isinP = do
  landCode <- count 2 letterChar
  nsin <- count 9 alphaNumChar
  checksum <- count 1 numberChar
  return $ Isin $ toText (landCode <> nsin <> checksum)

-- | The smart constructor for ISIN
--
-- >>> mkIsin "IE00B4L5Y983"
-- Just "IE00B4L5Y983"
mkIsin :: Text -> Maybe Isin
mkIsin = MP.parseMaybe (isinP <* MP.eof)
