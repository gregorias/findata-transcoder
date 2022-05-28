-- | This module parses Degiro CSV statement
module Transcoder.Degiro.Csv (
  -- * Parsing
  parseCsvStatement,

  -- * Types
  DegiroCsvRecord (..),
  DegiroIsin (..),
) where

import Control.Applicative.Combinators (count)
import qualified Data.ByteString.Lazy as LBS
import Data.Cash (Cash (Cash))
import Data.Csv (parseField, (.!))
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Either.Combinators (
  mapLeft,
 )
import Data.Time (
  Day,
  TimeOfDay (TimeOfDay),
  defaultTimeLocale,
  parseTimeM,
 )
import qualified Data.Vector as V
import Relude
import Text.Megaparsec (Parsec, single)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (numberChar)
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import Transcoder.Data.Isin (Isin)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (OptionalUnlimitedDecimalFraction),
  decimalP,
 )

-- | Type representing possible values of the ISIN field in Degiro reports.
data DegiroIsin
  = DegiroIsin !Isin
  | Nlflatexacnt
  deriving stock (Eq, Show)

instance Csv.FromField DegiroIsin where
  parseField field
    | field == encodeUtf8 @Text "NLFLATEXACNT" = return Nlflatexacnt
    | otherwise =
      (DegiroIsin <$> parseField field)
        <|> fail ("Expected NLFLATEXACNT or an ISIN, but got: " <> decodeUtf8 field <> ".")

newtype DegiroDay = DegiroDay
  { unDegiroDay :: Day
  }

instance Csv.FromField DegiroDay where
  parseField field =
    DegiroDay
      <$> parseTimeM True defaultTimeLocale "%d-%m-%Y" (decodeUtf8 field)

timeP :: MP.Parsec Void String TimeOfDay
timeP = do
  Just hours <- readMaybe <$> count 2 numberChar
  void $ single ':'
  Just minutes <- readMaybe <$> count 2 numberChar
  return $ TimeOfDay hours minutes 0

data DegiroCsvRecord = DegiroCsvRecord
  { dcrDate :: !Day
  , dcrTime :: !TimeOfDay
  , dcrValueDate :: !Day
  , dcrProduct :: !Text
  , dcrIsin :: !(Maybe DegiroIsin)
  , dcrDescription :: !Text
  , dcrFx :: !(Maybe Decimal)
  , dcrChange :: !(Maybe Cash)
  , dcrBalance :: !Cash
  , dcrOrderId :: !Text
  }
  deriving stock (Eq, Show)

degiroCashDecimalFormat :: DecimalFormat
degiroCashDecimalFormat = DecimalFormat NoChunkSep (Just OptionalUnlimitedDecimalFraction)

degiroCashP :: Parsec Void String Decimal
degiroCashP = decimalP degiroCashDecimalFormat

parseCash :: Csv.Field -> Csv.Field -> Csv.Parser (Maybe Cash)
parseCash "" "" = pure Nothing
parseCash changeCurrencyField changeAmountField = do
  changeCurrency <- Csv.parseField changeCurrencyField
  Just changeAmount <-
    MP.parseMaybe @Void degiroCashP
      <$> (Csv.parseField changeAmountField :: Csv.Parser String)
  return . Just $ Cash changeCurrency changeAmount

instance Csv.FromRecord DegiroCsvRecord where
  parseRecord rec = do
    date <- unDegiroDay <$> rec .! 0
    Just time <- MP.parseMaybe timeP <$> rec .! 1
    valueDate <- unDegiroDay <$> rec .! 2
    productStr <- rec .! 3
    maybeIsin <- rec .! 4
    desc <- rec .! 5
    fxStr :: String <- rec .! 6
    maybeFx <-
      if null fxStr
        then return Nothing
        else do
          Just fx <- return $ MP.parseMaybe @Void degiroCashP fxStr
          return $ Just fx
    maybeChange <- join $ parseCash <$> rec .! 7 <*> rec .! 8
    Just balance <- join $ parseCash <$> rec .! 9 <*> rec .! 10
    orderId <- rec .! 11
    return $
      DegiroCsvRecord
        date
        time
        valueDate
        productStr
        maybeIsin
        desc
        maybeFx
        maybeChange
        balance
        orderId

-- | Parses a Degiro CSV statement.
-- The left return value contains an error message.
parseCsvStatement :: CsvFile LBS.ByteString -> Either Text [DegiroCsvRecord]
parseCsvStatement (CsvFile content) = fmap V.toList . mapLeft toText $ Csv.decode Csv.HasHeader content