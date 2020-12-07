{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module contains parsers for CSV tables contained in Mark-to-Market an IB
-- statement.
module Hledupt.Ib.Csv.CsvParse
  ( Currency (..),
    PositionRecord (..),
    CashMovement (..),
    Statement (..),
    PositionRecordAssetClass (..),
    parse,
  )
where

import qualified Control.Lens as L
import Control.Monad (MonadPlus (mzero))
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Maybe (mapMaybe)
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM)
import qualified Data.Vector as V
import Hledupt.Data (MonetaryValue, myDecDec)
import Hledupt.Ib.Csv.RawParse (Csvs (..))
import Text.Megaparsec
  ( MonadParsec,
    Token,
    Tokens,
    anySingle,
    count,
    single,
    some,
    someTill_,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (digitChar, letterChar, space, string)

data Currency = USD | CHF
  deriving (Eq, Show)

instance Csv.FromField Currency where
  parseField "USD" = pure USD
  parseField "CHF" = pure CHF
  parseField _ = fail "Expected CHF/USD as currency"

-- Statement info parsers

monthParser ::
  ( MonadParsec e s m,
    Token s ~ Char,
    Tokens s ~ String
  ) =>
  m Int
monthParser = do
  monthString <- some letterChar
  case monthString of
    "January" -> return 1
    "February" -> return 2
    "March" -> return 3
    "April" -> return 4
    "May" -> return 5
    "June" -> return 6
    "July" -> return 7
    "August" -> return 8
    "September" -> return 9
    "October" -> return 10
    "November" -> return 11
    "December" -> return 12
    _ -> mzero

datePhraseParser :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Day
datePhraseParser = do
  month <- monthParser <* space
  day <- some digitChar <* string ", "
  year <- count 4 digitChar
  return $ fromGregorian (read year) month (read day)

periodPhraseParser :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Day
periodPhraseParser = do
  string "Period,\""
  date <-
    try (datePhraseParser >> string " - " >> datePhraseParser)
      <|> datePhraseParser
  single '"'
  return date

statementDateParser :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Day
statementDateParser = snd <$> someTill_ anySingle (try periodPhraseParser)

-- Positions AKA Status parsers

data PositionRecordAssetClass = Stocks | Forex
  deriving (Show, Eq)

instance Csv.FromField PositionRecordAssetClass where
  parseField "Stocks" = pure Stocks
  parseField "Forex" = pure Forex
  parseField _ = fail "Expected Stocks or Forex"

data PositionRecord = PositionRecord
  { prAssetClass :: PositionRecordAssetClass,
    prCurrency :: Currency,
    prSymbol :: String,
    prQuantity :: Decimal,
    prPrice :: MonetaryValue
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord PositionRecord where
  parseNamedRecord namedRecord =
    PositionRecord
      <$> lookupAux "Asset Class"
      <*> lookupAux "Currency"
      <*> lookupAux "Symbol"
      <*> (L.view myDecDec <$> lookupAux "Quantity")
      <*> (L.view myDecDec <$> lookupAux "Price")
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord

newtype PositionOrTotalRecord = PositionOrTotalRecord
  { potrPositionRecord :: Maybe PositionRecord
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord PositionOrTotalRecord where
  parseNamedRecord namedRecord =
    (PositionOrTotalRecord . Just <$> Csv.parseNamedRecord namedRecord)
      <|> pure (PositionOrTotalRecord Nothing)

-- Cash Movement section parsers

data CashMovement = CashMovement
  { cmDate :: Day,
    cmCurrency :: Currency,
    cmAmount :: MonetaryValue
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord CashMovement where
  parseNamedRecord namedRecord =
    CashMovement
      <$> (lookupAux "Settle Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
      <*> lookupAux "Currency"
      <*> (L.view myDecDec <$> lookupAux "Amount")
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord

newtype MaybeCashMovement = MaybeCashMovement
  { mcmCashMovement :: Maybe CashMovement
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord MaybeCashMovement where
  parseNamedRecord namedRecord =
    (MaybeCashMovement . Just <$> Csv.parseNamedRecord namedRecord)
      <|> pure (MaybeCashMovement Nothing)

-- | Useful information gleaned directly from IB's CSV statement.
data Statement = Statement
  { sLastStatementDay :: Day,
    sPositionRecords :: [PositionRecord],
    sCashMovements :: [CashMovement]
  }
  deriving (Eq, Show)

-- | Parses an M-to-M IB CSVs into individual data points and records.
parse :: Csvs -> Either String Statement
parse csvs = do
  date <- first show $ MP.parse statementDateParser "" (cStatement csvs)
  maybePositions :: [PositionOrTotalRecord] <-
    V.toList . snd <$> Csv.decodeByName (C.pack $ cPositions csvs)
  let cashCsv = cDepositsAndWithdrawals csvs
  maybeCashMovements :: [MaybeCashMovement] <-
    if null cashCsv
      then return []
      else
        V.toList . snd
          <$> Csv.decodeByName (C.pack cashCsv)
  let positions = mapMaybe potrPositionRecord maybePositions
  return $
    Statement
      date
      positions
      (mapMaybe mcmCashMovement maybeCashMovements)
