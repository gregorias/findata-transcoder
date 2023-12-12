-- | This module contains the DollarAmount type and its parsers.
module Transcoder.CharlesSchwab.DollarAmount (
  -- * Parsing
  dollarAmountP,

  -- * Types
  DollarAmount (..),
) where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Cash (Cash (Cash))
import Data.Csv qualified as Csv
import Data.Decimal (Decimal)
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  decimalP,
 )
import Data.Text qualified as T
import Hledger.Data.Extra (ToAmount (..))
import Relude
import Text.Megaparsec (Parsec, single)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (signed)
import Text.Megaparsec.Extra qualified as MP
import Transcoder.Data.Currency (usd)

-- | A wrapper around 'Decimal' to represent a dollar amount.
--
-- I would not need this type with dependent types. I would then just use Cash USD.
newtype DollarAmount = DollarAmount
  { unDollarAmount :: Decimal
  }
  deriving newtype (Eq, Ord, Show)

instance ToAmount DollarAmount where
  toAmount (DollarAmount amount) = toAmount (Cash usd amount)

unsignedDollarAmountP :: Parsec Void Text Decimal
unsignedDollarAmountP = do
  void $ single '$'
  decimalP (DecimalFormat (ChunkSep ',') (Just OptionalUnlimitedDecimalFraction))

dollarAmountP :: Parsec Void Text DollarAmount
dollarAmountP = DollarAmount <$> signed space unsignedDollarAmountP

instance Csv.FromField DollarAmount where
  parseField field = do
    let fieldString = decodeUtf8 field
    maybe
      (fail . toString $ "Could not parse the dollar amount: " <> fieldString)
      return
      (MP.parseMaybe dollarAmountP fieldString)

instance FromJSON DollarAmount where
  parseJSON = Aeson.withText "dollar amount" $ \text ->
    case MP.parsePretty dollarAmountP "" text of
      Left err -> fail $ "Could not parse the dollar amount: " <> T.unpack err
      Right amount -> return amount
