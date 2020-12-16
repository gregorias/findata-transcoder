{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module contains parsers for CSV tables contained in Mark-to-Market an IB
-- statement.
module Hledupt.Ib.Csv.CsvParse
  ( CashMovement (..),
    Currency (..),
    Dividend (..),
    Position (..),
    PositionAssetClass (..),
    Statement (..),
    WithholdingTax (..),
    parseMtmStatement,
  )
where

import qualified Control.Lens as L
import Control.Monad (MonadPlus (mzero), void)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Either.Combinators (maybeToRight)
import Data.Kind (Type)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM)
import qualified Data.Vector as V
import Data.Void (Void)
import Hledupt.Data (MonetaryValue, decimalParser, myDecDec)
import Hledupt.Ib.Csv.RawParse (Csv, CsvName, IbCsvs)
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
    some,
    someTill_,
    try,
    (<|>),
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

newtype IbCsvName a = IbCsvName
  { unIbCsvName :: String
  }
  deriving (IsString)

class FromIbCsvs d where
  type Record d :: Type
  csvName :: IbCsvName d
  fromRecord :: Record d -> Maybe d

data PositionAssetClass = Stocks | Forex
  deriving (Show, Eq)

instance Csv.FromField PositionAssetClass where
  parseField "Stocks" = pure Stocks
  parseField "Forex" = pure Forex
  parseField _ = fail "Expected Stocks or Forex"

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
  parseNamedRecord namedRecord =
    (PositionOrTotalRecord . Just <$> position)
      <|> pure (PositionOrTotalRecord Nothing)
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord
      position =
        Position
          <$> lookupAux "Asset Class"
          <*> lookupAux "Currency"
          <*> lookupAux "Symbol"
          <*> (L.view myDecDec <$> lookupAux "Quantity")
          <*> (L.view myDecDec <$> lookupAux "Price")

-- Cash Movement section parsers

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

instance Csv.FromNamedRecord CashMovementRecord where
  parseNamedRecord namedRecord =
    (CashMovementRecord . Just <$> cashMovement)
      <|> pure (CashMovementRecord Nothing)
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord
      cashMovement =
        CashMovement
          <$> (lookupAux "Settle Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
          <*> lookupAux "Currency"
          <*> (L.view myDecDec <$> lookupAux "Amount")

data Dividend = Dividend
  { dDate :: Day,
    dSymbol :: String,
    dDividendPerShare :: MonetaryValue,
    dTotalAmount :: MonetaryValue
  }
  deriving (Eq, Show)

instance FromIbCsvs Dividend where
  type Record Dividend = DividendRecord
  csvName = "Dividends"
  fromRecord = L.preview _DividendRecord

symbolParser ::
  (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m String
symbolParser = some letterChar

symbolDpsParser ::
  (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
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
  parseNamedRecord namedRecord = do
    currencyField <- Csv.lookup namedRecord "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalDividendsRecord
      else fmap DividendRecord dividend
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord
      dividendAux date (symbol, dps) total = Dividend date symbol dps total
      dividend =
        dividendAux
          <$> (lookupAux "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
          <*> ( do
                  desc :: String <- lookupAux "Description"
                  let parsed = MP.parse symbolDpsParser "" desc
                  either
                    ( \err ->
                        fail $
                          "Could not parse (symbol, dps).\n" ++ show err
                    )
                    return
                    parsed
              )
          <*> (L.view myDecDec <$> lookupAux "Amount")

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
  parseNamedRecord namedRecord = do
    currencyField <- Csv.lookup namedRecord "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalWithholdingTaxRecord
      else WithholdingTaxRecord <$> withholdingTax
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord
      withholdingTax =
        do
          WithholdingTax
          <$> (lookupAux "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
            <*> ( do
                    desc :: String <- lookupAux "Description"
                    let parsed = MP.parse symbolParser "" desc
                    either
                      ( \err ->
                          fail $
                            "Could not parse symbol.\n" ++ show err
                      )
                      return
                      parsed
                )
            <*> (L.view myDecDec <$> lookupAux "Amount")

_WithholdingTaxRecord :: L.Prism' WithholdingTaxRecord WithholdingTax
_WithholdingTaxRecord =
  L.prism'
    WithholdingTaxRecord
    ( \case
        TotalWithholdingTaxRecord -> Nothing
        WithholdingTaxRecord tax -> Just tax
    )

-- | Useful information gleaned directly from IB's CSV statement.
data Statement = Statement
  { sLastStatementDay :: Day,
    sPositions :: [Position],
    sCashMovements :: [CashMovement],
    sDividends :: [Dividend],
    sWithholdingTaxes :: [WithholdingTax]
  }
  deriving (Eq, Show)

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
      ("Could not parse " ++ name ++ "records.")
      $ parseCsv csv
  return $ mapMaybe l fullRecords

-- TODO Delete CsvParser, rely on FromIbCsvs
dividendsParser :: CsvParser DividendRecord Dividend
dividendsParser =
  CsvParser
    { _cpCsvName = unIbCsvName (csvName :: IbCsvName Dividend),
      _cpPrism = fromRecord
    }

-- | Parses an M-to-M IB CSVs into individual data points and records.
parseMtmStatement :: IbCsvs -> Either String Statement
parseMtmStatement csvs = do
  date <- do
    stmtCsv <- fetchCsv "Statement" csvs
    first errorBundlePretty $ MP.parse statementDateParser "" stmtCsv

  let positionCsv = fetchCsvOrEmpty "Positions and Mark-to-Market Profit and Loss" csvs
  maybePositions :: [PositionOrTotalRecord] <-
    prependErrorMessage "Could not parse cash position records." $
      parseCsv positionCsv

  let cashCsv = fetchCsvOrEmpty "Deposits & Withdrawals" csvs
  maybeCashMovements :: [CashMovementRecord] <-
    prependErrorMessage "Could not parse cash movement data." $
      parseCsv cashCsv

  dividends <- parseCsv' dividendsParser csvs

  let taxCsv = fetchCsvOrEmpty "Withholding Tax" csvs
  taxes <-
    fmap (mapMaybe (L.preview _WithholdingTaxRecord))
      <$> prependErrorMessage "Could not parse taxes data."
      $ parseCsv taxCsv

  let positions = mapMaybe potrPosition maybePositions
  return $
    Statement
      date
      positions
      (mapMaybe cmrCashMovement maybeCashMovements)
      dividends
      taxes
