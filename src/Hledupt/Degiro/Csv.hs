{-# LANGUAGE OverloadedStrings #-}

-- | This module parses Degiro CSV statement
module Hledupt.Degiro.Csv (
  -- * Parsing
  parseCsvStatement,

  -- * Types
  DegiroCsvRecord (..),
) where

import Control.Applicative.Combinators (count)
import Control.Lens.Internal.ByteString (unpackStrict8)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv ((.!))
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time (
  Day,
  TimeOfDay (TimeOfDay),
  defaultTimeLocale,
  parseTimeM,
 )
import Data.Vector (Vector)
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.CsvFile (CsvFile (CsvFile))
import Hledupt.Data.Isin (Isin, mkIsin)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (OptionalUnlimitedDecimalFraction),
  decimalP,
 )
import Relude
import Text.Megaparsec (Parsec, single)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (numberChar)

newtype DegiroDay = DegiroDay
  { unDegiroDay :: Day
  }

instance Csv.FromField DegiroDay where
  parseField field =
    DegiroDay
      <$> parseTimeM True defaultTimeLocale "%d-%m-%Y" (unpackStrict8 field)

timeP :: MP.Parsec Void String TimeOfDay
timeP = do
  Just hours <- readMaybe <$> count 2 numberChar
  void $ single ':'
  Just minutes <- readMaybe <$> count 2 numberChar
  return $ TimeOfDay hours minutes 0

data DegiroCsvRecord = DegiroCsvRecord
  { dcrDate :: Day
  , dcrTime :: TimeOfDay
  , dcrValueDate :: Day
  , dcrProduct :: Text
  , dcrIsin :: Maybe Isin
  , dcrDescription :: Text
  , dcrFx :: Maybe Decimal
  , dcrChange :: Maybe Cash
  , dcrBalance :: Cash
  , dcrOrderId :: Text
  }
  deriving stock (Eq, Ord, Show)

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
    isinStr <- rec .! 4
    maybeIsin <-
      if Text.null isinStr
        then return Nothing
        else do
          Just isin <- return $ mkIsin isinStr
          return $ Just isin
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
parseCsvStatement :: CsvFile LBS.ByteString -> Either String (Vector DegiroCsvRecord)
parseCsvStatement (CsvFile content) = Csv.decode Csv.HasHeader content
