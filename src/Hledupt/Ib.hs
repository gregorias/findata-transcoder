{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledupt.Ib
  ( ibCsvToLedger,
    IbCsvs (..),
    rawStatementParser,
    PositionRecord (..),
    PositionRecordCurrency (..),
    PositionRecordAssetClass (..),
  )
where

import qualified Control.Lens as L
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.List (partition)
import Hledupt.Data (MonetaryValue, myDecDec)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    many,
    noneOf,
    notFollowedBy,
    some,
  )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Extra (eolOrEof)

-- Parsing (Raw Account Statement → IbCsvs)

type Csv = String

data CsvLine = CsvLine
  { header :: String,
    remainingLine :: String
  }
  deriving (Eq, Show)

data IbCsvs = IbCsvs
  { statement :: Csv,
    positions :: Csv
  }
  deriving (Eq, Show)

type IbParser = Parsec () String

rawStatementLineParser :: IbParser CsvLine
rawStatementLineParser = do
  headerString <- many $ noneOf ("," :: String)
  void $ char ','
  rest <- some $ notFollowedBy eolOrEof *> anySingle
  end <- eolOrEof
  return $ CsvLine headerString (rest ++ end)

rawStatementParser :: IbParser IbCsvs
rawStatementParser = do
  ibCsvLines <- many rawStatementLineParser
  let (stmtLines, nonStmtLines) = partition ((== "Statement") . header) ibCsvLines
      statusLines = filter ((== "Positions and Mark-to-Market Profit and Loss") . header) nonStmtLines
      stmtCsv = concatMap remainingLine stmtLines
      statusCsv = concatMap remainingLine statusLines
  return $ IbCsvs {statement = stmtCsv, positions = statusCsv}

-- Parsing (IbCsvs → Status)

data PositionRecordAssetClass = Stocks | Forex | Other
  deriving (Show, Eq)

data PositionRecordCurrency = USD | CHF
  deriving (Show, Eq)

data PositionRecord = PositionRecord
  { assetClass :: PositionRecordAssetClass,
    currency :: PositionRecordCurrency,
    symbol :: String,
    description :: String,
    quantity :: Decimal,
    price :: MonetaryValue
  }
  deriving (Show, Eq)

instance Csv.FromField PositionRecordAssetClass where
  parseField "Stocks" = pure Stocks
  parseField "Forex" = pure Forex
  parseField _ = pure Other

instance Csv.FromField PositionRecordCurrency where
  parseField "USD" = pure USD
  parseField "CHF" = pure CHF
  parseField _ = fail "Expected CHF/USD as currency"

instance Csv.FromNamedRecord PositionRecord where
  parseNamedRecord namedRecord =
    PositionRecord
      <$> lookupAux "Asset Class"
      <*> lookupAux "Currency"
      <*> lookupAux "Symbol"
      <*> lookupAux "Description"
      <*> (L.view myDecDec <$> lookupAux "Quantity")
      <*> (L.view myDecDec <$> lookupAux "Price")
    where
      lookupAux :: Csv.FromField a => BS.ByteString -> Csv.Parser a
      lookupAux = Csv.lookup namedRecord

ibCsvToLedger :: String -> String
ibCsvToLedger _ = "Could not parse the CSV, because the functionality is unimplemented"
