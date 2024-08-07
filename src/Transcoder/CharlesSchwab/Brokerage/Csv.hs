-- | Utilities for parsing Charles Schwab's brokerage account history CSV.
module Transcoder.CharlesSchwab.Brokerage.Csv (
  -- * Parsing
  parseBrokerageHistoryCsv,

  -- * Types
  BrokerageHistoryCsvRecord (..),
) where

import Control.Monad.Combinators (manyTill)
import Data.ByteString.Lazy qualified as LBS
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Data.Decimal (Decimal)
import Data.Either.Extra (mapLeft)
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Relude
import Text.Megaparsec (Parsec, anySingle)
import Text.Megaparsec qualified as MP
import Transcoder.CharlesSchwab.DollarAmount (DollarAmount, dollarAmountP)

csSimpleDayP :: Parsec Void String Day
csSimpleDayP = do
  content <- many anySingle
  parseTimeM True defaultTimeLocale "%m/%d/%Y" content

csAsOfDayP :: Parsec Void String Day
csAsOfDayP = do
  content <- manyTill anySingle (MP.single ' ')
  void MP.takeRest
  parseTimeM True defaultTimeLocale "%m/%d/%Y" content

csDayP :: Parsec Void String Day
csDayP = MP.try csSimpleDayP <|> csAsOfDayP

newtype CsDay = CsDay
  { unCsDay :: Day
  }

instance Csv.FromField CsDay where
  parseField field = do
    let (day :: Maybe Day) = MP.parseMaybe csDayP (decodeUtf8 field)
    maybe (fail $ "Could not parse the date field: " ++ decodeUtf8 field) (return . CsDay) day

quantityP :: Csv.NamedRecord -> Csv.Parser (Maybe Decimal)
quantityP rec = do
  field <- rec .: "Quantity"
  if field == ""
    then pure Nothing
    else return $ readMaybe field

-- | A CSV record from Charles Schwab's brokerage account history.
data BrokerageHistoryCsvRecord = BrokerageHistoryCsvRecord
  { bhcrDate :: !Day
  , bhcrAction :: !Text
  , bhcrSymbol :: !Text
  , bhcrDescription :: !Text
  , bhcrQuantity :: !(Maybe Decimal)
  , bhcrPrice :: !(Maybe DollarAmount)
  , bhcrFees :: !(Maybe DollarAmount)
  , bhcrAmount :: !(Maybe DollarAmount)
  }
  deriving stock (Eq, Ord, Show)

instance FromNamedRecord BrokerageHistoryCsvRecord where
  parseNamedRecord rec =
    (BrokerageHistoryCsvRecord . unCsDay <$> (rec .: "Date"))
      <*> rec
      .: "Action"
      <*> rec
      .: "Symbol"
      <*> rec
      .: "Description"
      <*> quantityP rec
      <*> maybeDollarAmountP rec "Price"
      <*> maybeDollarAmountP rec "Fees & Comm"
      <*> rec
      .: "Amount"

maybeDollarAmountP :: Csv.NamedRecord -> ByteString -> Csv.Parser (Maybe DollarAmount)
maybeDollarAmountP rec name = do
  field <- rec .: name
  if field == ""
    then pure Nothing
    else return $ MP.parseMaybe dollarAmountP field

-- | Parses a Charles Schwab brokerage account history statement.
parseBrokerageHistoryCsv :: LBS.ByteString -> Either Text [BrokerageHistoryCsvRecord]
parseBrokerageHistoryCsv stmt = do
  toList . snd <$> mapLeft toText (Csv.decodeByName stmt)
