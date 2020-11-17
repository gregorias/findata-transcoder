{-# LANGUAGE UnicodeSyntax #-}
module Bcge(
bcgeCsvToLedger,
  BcgeTransaction(..),
)
where

import           Control.Monad        (void)
import           Data                 (MonetaryValue, fromUnitsAndCents)
import           Data.List            (elemIndex)
import           Data.Time.Calendar   (Day)
import           Text.Megaparsec      (Parsec, eof, many, noneOf, oneOf, parse,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol, string)

-- Parser functionality (CSV String → [BcgeTransaction])

type CsvLine = (String, String, String)
type Header = [CsvLine]
type Content = [CsvLine]
type BcgeParser = Parsec () String

bcgeCsvLineParser :: BcgeParser CsvLine
bcgeCsvLineParser = do
  f0 <- many (noneOf ";") <* char ';'
  f1 <- many (noneOf ";") <* char ';'
  f2 <- many (noneOf ";") <* char ';'
  many (noneOf "\r\n") <* (void eol <|> eof)
  return (f0, f1, f2)

bcgeCsvParser :: BcgeParser [CsvLine]
bcgeCsvParser = many bcgeCsvLineParser

-- Parsed Strings → Safe data (BCGE transactions)

-- | BCGE's transaction data fetched from their website.
data BcgeTransaction = BcgeTransaction{
  bTrDate     :: Day
  , bTrTitle  :: String
  , bTrAmount :: MonetaryValue
}
                                         deriving (Eq, Show)

data BcgeStatement = BcgeStatement{
  bStatementBalance        :: MonetaryValue
  , bStatementTransactions :: [BcgeTransaction]
}

headerLine :: CsvLine
headerLine = ("Datum", "Buchungstext", "Betrag")

splitCsv :: [CsvLine] -> (Header, Content)
splitCsv csvLines = (take headerPos csvLines, drop (headerPos + 1) csvLines)
  where
    Just headerPos = elemIndex headerLine csvLines

getSaldo :: [CsvLine] -> [String]
getSaldo header = [f0 | (f0, _, _) <- header, take 5 f0 == "Saldo"]

bcgeCsvToLedger :: String → String
bcgeCsvToLedger input =
  let
    Right lines = parse bcgeCsvParser "" input
    (header, content) = splitCsv lines
  in show (getSaldo header, content)
