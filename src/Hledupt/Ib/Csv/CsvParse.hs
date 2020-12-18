{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module contains parsers for CSV tables of the following IB statements:
--
-- * Activity Statement
-- * Mark-to-Market Statement
module Hledupt.Ib.Csv.CsvParse
  ( CashMovement (..),
    ActivityStatement (..),
    Currency (..),
    Dividend (..),
    EndingCash (..),
    MtmStatement (..),
    Position (..),
    PositionAssetClass (..),
    StockPosition (..),
    Trade (..),
    WithholdingTax (..),
    parseActivityStatement,
    parseMtmStatement,
  )
where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv hiding (FromNamedRecord, decodeByName, lookup)
import qualified Data.Csv.Extra as Csv
import Data.Decimal (Decimal)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM)
import qualified Data.Vector as V
import Hledupt.Data (MonetaryValue, decimalParser, myDecDec)
import Hledupt.Ib.Csv.RawParse (Csv, CsvName, IbCsvs)
import Relude
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    Token,
    Tokens,
    anySingle,
    count,
    errorBundlePretty,
    label,
    single,
    someTill_,
    try,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space, string)

data Currency = USD | CHF
  deriving (Eq, Show)

instance Csv.FromField Currency where
  parseField "USD" = pure USD
  parseField "CHF" = pure CHF
  parseField _ = fail "Expected CHF/USD as currency"

-- Statement info parsers

monthParser :: Parsec Void String Int
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

datePhraseParser :: Parsec Void String Day
datePhraseParser = do
  month <- monthParser <* space
  day <- some digitChar <* string ", "
  year <- count 4 digitChar
  let date = do
        yearInt <- readMaybe year
        dayInt <- readMaybe day
        return $ fromGregorian yearInt month dayInt
  case date of
    Just d -> return d
    Nothing -> fail "Could not parse date"

periodPhraseParser :: Parsec Void String Day
periodPhraseParser = do
  void $ string "Period,\""
  date <-
    try (datePhraseParser >> string " - " >> datePhraseParser)
      <|> datePhraseParser
  void $ single '"'
  return date

statementDateParser :: Parsec Void String Day
statementDateParser = snd <$> someTill_ anySingle (try periodPhraseParser)

-- Positions AKA Status parsers

data CsvParser recordType dataType = CsvParser
  { _cpCsvName :: String,
    _cpPrism :: recordType -> Maybe dataType
  }

data PositionAssetClass = Stocks | Forex
  deriving (Show, Eq)

instance Csv.FromField PositionAssetClass where
  parseField "Stocks" = pure Stocks
  parseField "Forex" = pure Forex
  parseField _ = fail "Expected Stocks or Forex"

data StockPosition = StockPosition
  { spSymbol :: String,
    spQuantity :: Integer,
    spPrice :: MonetaryValue
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord OpenPositionsRecord where
  parseNamedRecord = do
    header <- Csv.lookup "Header"
    if header == "Total"
      then return TotalOpenPositionsRecord
      else StockPositionRecord <$> stockPosition
    where
      stockPosition =
        StockPosition <$> Csv.lookup "Symbol"
          <*> Csv.lookup "Quantity"
          <*> (L.view myDecDec <$> Csv.lookup "Close Price")

data OpenPositionsRecord
  = StockPositionRecord StockPosition
  | TotalOpenPositionsRecord

stockPositionParser :: CsvParser OpenPositionsRecord StockPosition
stockPositionParser =
  CsvParser
    { _cpCsvName = "Open Positions",
      _cpPrism = \case
        TotalOpenPositionsRecord -> Nothing
        StockPositionRecord sp -> Just sp
    }

data Position = Position
  { prAssetClass :: PositionAssetClass,
    prCurrency :: Currency,
    prSymbol :: String,
    prQuantity :: Decimal,
    prPrice :: MonetaryValue
  }
  deriving (Eq, Show)

newtype PositionOrTotalRecord = PositionOrTotalRecord
  { potrPosition :: Maybe Position
  }
  deriving (Eq, Show)

instance Csv.FromNamedRecord PositionOrTotalRecord where
  parseNamedRecord =
    (PositionOrTotalRecord . Just <$> position)
      <|> pure (PositionOrTotalRecord Nothing)
    where
      position =
        Position
          <$> Csv.lookup "Asset Class"
          <*> Csv.lookup "Currency"
          <*> Csv.lookup "Symbol"
          <*> (L.view myDecDec <$> Csv.lookup "Quantity")
          <*> (L.view myDecDec <$> Csv.lookup "Price")

mtmPositionsParser :: CsvParser PositionOrTotalRecord Position
mtmPositionsParser =
  CsvParser
    { _cpCsvName = "Positions and Mark-to-Market Profit and Loss",
      _cpPrism = potrPosition
    }

data Trade = Trade
  { tradeDate :: Day,
    tradeSymbol :: String,
    tradeQuantity :: Integer,
    tradeAmount :: MonetaryValue,
    tradeFee :: MonetaryValue
  }
  deriving (Eq, Show)

data TradeRecord
  = DataTradeRecord Trade
  | TotalTradeRecord

instance Csv.FromNamedRecord TradeRecord where
  parseNamedRecord = do
    header <- Csv.lookup "Header"
    if header `elem` ["SubTotal", "Total"]
      then return TotalTradeRecord
      else
        DataTradeRecord
          <$> ( Trade
                  <$> ( do
                          dtString <- Csv.lookup "Date/Time"
                          let eitherDate = MP.parse dateTimeParser "" dtString
                          either (const mzero) return eitherDate
                      )
                  <*> Csv.lookup "Symbol"
                  <*> Csv.lookup "Quantity"
                  <*> (L.view myDecDec <$> Csv.lookup "Proceeds")
                  <*> (L.view myDecDec <$> Csv.lookup "Comm/Fee")
              )
    where
      dateTimeParser :: Parsec Void String Day
      dateTimeParser =
        MP.manyTill anySingle (single ',')
          >>= parseTimeM True defaultTimeLocale "%Y-%m-%d"

tradeParser :: CsvParser TradeRecord Trade
tradeParser =
  CsvParser
    { _cpCsvName = "Trades",
      _cpPrism = \case
        TotalTradeRecord -> Nothing
        DataTradeRecord t -> Just t
    }

data CashMovement = CashMovement
  { cmDate :: Day,
    cmCurrency :: Currency,
    cmAmount :: MonetaryValue
  }
  deriving (Eq, Show)

newtype CashMovementRecord = CashMovementRecord
  { cmrCashMovement :: Maybe CashMovement
  }
  deriving (Eq, Show)

_CashMovementRecord :: L.Prism' CashMovementRecord CashMovement
_CashMovementRecord =
  L.prism'
    (CashMovementRecord . Just)
    cmrCashMovement

instance Csv.FromNamedRecord CashMovementRecord where
  parseNamedRecord =
    (CashMovementRecord . Just <$> cashMovement)
      <|> pure (CashMovementRecord Nothing)
    where
      cashMovement =
        CashMovement
          <$> (Csv.lookup "Settle Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
          <*> Csv.lookup "Currency"
          <*> (L.view myDecDec <$> Csv.lookup "Amount")

depositsAndWithdrawalsParser :: CsvParser CashMovementRecord CashMovement
depositsAndWithdrawalsParser =
  CsvParser
    { _cpCsvName = "Deposits & Withdrawals",
      _cpPrism = L.preview _CashMovementRecord
    }

data Dividend = Dividend
  { dDate :: Day,
    dSymbol :: String,
    dDividendPerShare :: MonetaryValue,
    dTotalAmount :: MonetaryValue
  }
  deriving (Eq, Show)

symbolParser ::
  (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m String
symbolParser = some letterChar

symbolDpsParser ::
  (MonadFail m, MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m (String, MonetaryValue)
symbolDpsParser = do
  symbol <- symbolParser
  void $ label "ISIN" $ char '(' >> some alphaNumChar >> char ')'
  void $ string " Cash Dividend USD "
  dps <- decimalParser
  void $ string " per Share (Ordinary Dividend)"
  return (symbol, dps)

data DividendRecord
  = DividendRecord Dividend
  | TotalDividendsRecord
  deriving (Eq, Show)

_DividendRecord :: L.Prism' DividendRecord Dividend
_DividendRecord =
  L.prism'
    DividendRecord
    ( \case
        TotalDividendsRecord -> Nothing
        DividendRecord dividend -> Just dividend
    )

instance Csv.FromNamedRecord DividendRecord where
  parseNamedRecord = do
    currencyField <- Csv.lookup "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalDividendsRecord
      else fmap DividendRecord dividend
    where
      dividendAux date (symbol, dps) total = Dividend date symbol dps total
      dividend =
        dividendAux
          <$> (Csv.lookup "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
          <*> ( do
                  desc :: String <- Csv.lookup "Description"
                  let parsed = MP.parse symbolDpsParser "" desc
                  either
                    ( \err ->
                        fail $
                          "Could not parse (symbol, dps).\n" ++ show err
                    )
                    return
                    parsed
              )
          <*> (L.view myDecDec <$> Csv.lookup "Amount")

dividendsParser :: CsvParser DividendRecord Dividend
dividendsParser =
  CsvParser
    { _cpCsvName = "Dividends",
      _cpPrism = L.preview _DividendRecord
    }

data WithholdingTax = WithholdingTax
  { wtDate :: Day,
    wtSymbol :: String,
    wtTotalAmount :: MonetaryValue
  }
  deriving (Eq, Show)

-- | A record that appears in "Withholding Tax" CSV
data WithholdingTaxRecord
  = WithholdingTaxRecord WithholdingTax
  | TotalWithholdingTaxRecord
  deriving (Eq, Show)

instance Csv.FromNamedRecord WithholdingTaxRecord where
  parseNamedRecord = do
    currencyField <- Csv.lookup "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalWithholdingTaxRecord
      else WithholdingTaxRecord <$> withholdingTax
    where
      withholdingTax =
        do
          WithholdingTax
          <$> (Csv.lookup "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
            <*> ( do
                    desc :: String <- Csv.lookup "Description"
                    let parsed = MP.parse symbolParser "" desc
                    either
                      ( \err ->
                          fail $
                            "Could not parse symbol.\n" ++ show err
                      )
                      return
                      parsed
                )
            <*> (L.view myDecDec <$> Csv.lookup "Amount")

_WithholdingTaxRecord :: L.Prism' WithholdingTaxRecord WithholdingTax
_WithholdingTaxRecord =
  L.prism'
    WithholdingTaxRecord
    ( \case
        TotalWithholdingTaxRecord -> Nothing
        WithholdingTaxRecord tax -> Just tax
    )

withholdingTaxParser :: CsvParser WithholdingTaxRecord WithholdingTax
withholdingTaxParser =
  CsvParser
    { _cpCsvName = "Withholding Tax",
      _cpPrism = L.preview _WithholdingTaxRecord
    }

data EndingCash = EndingCash
  { ecCurrency :: String,
    ecAmount :: MonetaryValue
  }
  deriving (Eq, Show)

data CashReportRecord
  = EndingCashRecord EndingCash
  | OtherCashReportRecord
  deriving (Eq, Show)

instance Csv.FromNamedRecord CashReportRecord where
  parseNamedRecord = do
    currencySummaryField <- Csv.lookup "Currency Summary"
    currencyField <- Csv.lookup "Currency"
    if currencySummaryField /= "Ending Cash" || currencyField == "Base Currency Summary"
      then return OtherCashReportRecord
      else
        EndingCashRecord . EndingCash currencyField
          <$> (L.view myDecDec <$> Csv.lookup "Total")

cashReportParser :: CsvParser CashReportRecord EndingCash
cashReportParser =
  CsvParser
    { _cpCsvName = "Cash Report",
      _cpPrism = \case
        OtherCashReportRecord -> Nothing
        EndingCashRecord ec -> Just ec
    }

fetchCsv :: CsvName -> IbCsvs -> Either String Csv
fetchCsv name =
  maybeToRight ("Could not find " ++ name ++ " among the CSVs.")
    . Map.lookup name

fetchCsvOrEmpty :: CsvName -> IbCsvs -> Csv
fetchCsvOrEmpty = Map.findWithDefault ""

parseCsv :: (Csv.FromNamedRecord a) => String -> Either String [a]
parseCsv csv =
  if null csv
    then return []
    else V.toList . snd <$> Csv.decodeByName (C.pack csv)

prependErrorMessage :: String -> Either String a -> Either String a
prependErrorMessage err = L._Left L.%~ (errln ++)
  where
    errln = err ++ "\n"

parseCsv' :: (Csv.FromNamedRecord a) => CsvParser a b -> IbCsvs -> Either String [b]
parseCsv' (CsvParser name l) csvs = do
  let csv = fetchCsvOrEmpty name csvs
  fullRecords <-
    prependErrorMessage
      ("Could not parse " ++ name ++ " records.")
      $ parseCsv csv
  return $ mapMaybe l fullRecords

-- | Useful information gleaned directly from IB's CSV activity statement.
data ActivityStatement = ActivityStatement
  { asLastStatementDay :: Day,
    asCashPositions :: [EndingCash],
    asStockPositions :: [StockPosition],
    asCashMovements :: [CashMovement],
    asTrades :: [Trade],
    asDividends :: [Dividend],
    asTaxes :: [WithholdingTax]
  }
  deriving (Eq, Show)

parseActivityStatement :: IbCsvs -> Either String ActivityStatement
parseActivityStatement csvs = do
  date <- do
    stmtCsv <- fetchCsv "Statement" csvs
    first errorBundlePretty $ MP.parse statementDateParser "" stmtCsv
  cashPositions <- parseCsv' cashReportParser csvs
  stockPositions <- parseCsv' stockPositionParser csvs
  trades <- parseCsv' tradeParser csvs
  dividends <- parseCsv' dividendsParser csvs
  taxes <- parseCsv' withholdingTaxParser csvs
  return $
    ActivityStatement
      date
      cashPositions
      stockPositions
      []
      trades
      dividends
      taxes

-- | Useful information gleaned directly from IB's MtM CSV statement.
data MtmStatement = MtmStatement
  { mtmsLastStatementDay :: Day,
    mtmsPositions :: [Position],
    mtmsCashMovements :: [CashMovement],
    mtmsDividends :: [Dividend],
    mtmsWithholdingTaxes :: [WithholdingTax]
  }
  deriving (Eq, Show)

-- | Parses an M-to-M IB CSVs into individual data points and records.
parseMtmStatement :: IbCsvs -> Either String MtmStatement
parseMtmStatement csvs = do
  date <- do
    stmtCsv <- fetchCsv "Statement" csvs
    first errorBundlePretty $ MP.parse statementDateParser "" stmtCsv
  positions <- parseCsv' mtmPositionsParser csvs
  cashMovements <- parseCsv' depositsAndWithdrawalsParser csvs
  dividends <- parseCsv' dividendsParser csvs
  taxes <- parseCsv' withholdingTaxParser csvs
  return $
    MtmStatement
      date
      positions
      cashMovements
      dividends
      taxes
