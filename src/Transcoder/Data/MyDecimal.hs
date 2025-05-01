-- | My extended parsing utilities for parsing decimals.
module Transcoder.Data.MyDecimal (
  myDecDec,
  MyDecimal (..),
) where

import Control.Lens.Iso qualified as Lens
import Data.Csv qualified as Csv
import Data.Decimal (Decimal)
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  decimalP,
 )
import Relude
import Text.Megaparsec (parseMaybe)

-- | A Decimal wrapper that is extended with parsing functions.
newtype MyDecimal = MyDecimal {unMyDecimal :: Decimal}
  deriving stock (Eq, Show)

myDecDec :: Lens.Iso' MyDecimal Decimal
myDecDec = Lens.iso (\(MyDecimal d) -> d) MyDecimal

instance Csv.FromField MyDecimal where
  parseField field =
    maybe
      ( fail
          $ "Could not parse the string \""
          <> decodeUtf8 field
          <> "\" as a decimal."
      )
      (pure . MyDecimal)
      $ parseMaybe @Void @String
        (decimalP (DecimalFormat (ChunkSep ',') (Just OptionalUnlimitedDecimalFraction)))
        (decodeUtf8 field)
