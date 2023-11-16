-- | This module contains utilities for parsing Charles Schwab's CSVs.
module Transcoder.CharlesSchwab.Csv (
  -- * Parsing
  dollarAmountP,

  -- * Types
  DollarAmount (..),
) where

import Data.Csv qualified as Csv
import Data.Decimal (Decimal)
import Relude
import Text.Megaparsec (Parsec, single)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (signed)
import Transcoder.Data.MyDecimal (
  decimalP,
  defaultDecimalFormat,
 )

-- I would not need this type with dependent types.
-- I would just use (Cash USD).
newtype DollarAmount = DollarAmount
  { unDollarAmount :: Decimal
  }
  deriving newtype (Eq, Ord, Show)

unsignedDollarAmountP :: Parsec Void Text Decimal
unsignedDollarAmountP = do
  void $ single '$'
  decimalP defaultDecimalFormat

dollarAmountP :: Parsec Void Text DollarAmount
dollarAmountP = DollarAmount <$> signed space unsignedDollarAmountP

instance Csv.FromField DollarAmount where
  parseField field = do
    let fieldString = decodeUtf8 field
    maybe
      (fail . toString $ "Could not parse the dollar amount: " <> fieldString)
      return
      (MP.parseMaybe dollarAmountP fieldString)
