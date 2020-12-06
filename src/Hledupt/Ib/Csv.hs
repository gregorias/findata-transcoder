{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib.Csv
  ( PositionRecordAssetClass (..),
    Currency (..),
    PositionRecord (..),
    CashMovement (..),
    Statement (..),
    parse,
  )
where

import Control.Applicative
import qualified Control.Lens as L
import Control.Monad (MonadPlus (mzero))
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Data.Time (parseTimeM)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (defaultTimeLocale)
import qualified Data.Vector as V
import Hledupt.Data (MonetaryValue, myDecDec)
import Text.Megaparsec
  ( MonadParsec,
    Token,
    Tokens,
    anySingle,
    count,
    eof,
    single,
    try,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, printChar, string)
import Text.Megaparsec.Char.Extra (bom, space)

-- Parsing (Raw Account Statement → Csvs)

type Csv = String

data CsvLine = CsvLine
  { header :: String,
    remainingLine :: String
  }

data Csvs = Csvs
  { statement :: Csv,
    positions :: Csv,
    depositsAndWithdrawals :: Csv
  }
  deriving (Show)

rawStatementLineParser ::
  (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m CsvLine
rawStatementLineParser = do
  headerString <- MP.someTill printChar (char ',')
  rest <- MP.some printChar
  end <- try eol <|> return ""
  return $ CsvLine headerString (rest ++ end)

rawStatementParser :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Csvs
rawStatementParser = do
  optional bom
  ibCsvLines <- many rawStatementLineParser
  let (stmtLines, nonStmtLines) = partition ((== "Statement") . header) ibCsvLines
      (statusLines, nonStatutNorStmtLines) =
        partition
          ((== "Positions and Mark-to-Market Profit and Loss") . header)
          nonStmtLines
      cashLines =
        filter
          ((== "Deposits & Withdrawals") . header)
          nonStatutNorStmtLines
      stmtCsv = concatMap remainingLine stmtLines
      statusCsv = concatMap remainingLine statusLines
      cashCsv = concatMap remainingLine cashLines
  eof
  return $
    Csvs
      { statement = stmtCsv,
        positions = statusCsv,
        depositsAndWithdrawals = cashCsv
      }

-- Proper CSV parsing (Csvs → data and records)

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
statementDateParser = snd <$> MP.someTill_ anySingle (try periodPhraseParser)

data PositionRecordAssetClass = Stocks | Forex
  deriving (Show, Eq)

instance Csv.FromField PositionRecordAssetClass where
  parseField "Stocks" = pure Stocks
  parseField "Forex" = pure Forex
  parseField _ = fail "Expected Stocks or Forex"

data Currency = USD | CHF
  deriving (Eq, Show)

instance Csv.FromField Currency where
  parseField "USD" = pure USD
  parseField "CHF" = pure CHF
  parseField _ = fail "Expected CHF/USD as currency"

data PositionRecord = PositionRecord
  { assetClass :: PositionRecordAssetClass,
    currency :: Currency,
    symbol :: String,
    quantity :: Decimal,
    price :: MonetaryValue
  }
  deriving (Eq, Show)

newtype PositionOrTotalRecord = PositionOrTotalRecord
  { positionRecord :: Maybe PositionRecord
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

instance Csv.FromNamedRecord PositionOrTotalRecord where
  parseNamedRecord namedRecord =
    (PositionOrTotalRecord . Just <$> Csv.parseNamedRecord namedRecord)
      <|> pure (PositionOrTotalRecord Nothing)

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
  { cashMovement :: Maybe CashMovement
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

-- | Parses an M-to-M IB CSV statement into individual data points and records.
parse :: String -> Either String Statement
parse csv = do
  let parsecErrToString :: (Show a) => Either a b -> Either String b
      parsecErrToString = first show
  csvs <- parsecErrToString $ MP.parse rawStatementParser "" csv
  date <-
    parsecErrToString (MP.parse statementDateParser "" (statement csvs))
      <|> Left "Could not parse the statement's date."
  maybePositions :: [PositionOrTotalRecord] <-
    (V.toList . snd <$> Csv.decodeByName (C.pack $ positions csvs))
      <|> Left "Could not parse positions."
  let cashCsv = depositsAndWithdrawals csvs
  maybeCashMovements <-
    if null cashCsv
      then return []
      else
        ( V.toList . snd
            <$> Csv.decodeByName (C.pack cashCsv)
        )
          <|> Left "Could not parse cashMovements."

  let positions = mapMaybe positionRecord maybePositions
  return $
    Statement
      date
      positions
      (mapMaybe cashMovement maybeCashMovements)
