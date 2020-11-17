{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
module Bcge(
bcgeCsvToLedger,
  BcgeTransaction(..),
  CsvLine(..),
  csvLineToBcgeTransaction,
)
where

import           Control.Monad        (void)
import           Data                 (MonetaryValue, fromUnitsAndCents)
import           Data.List            (elemIndex)
import           Data.Time.Calendar   (Day)
import           Data.Time.Format     (defaultTimeLocale, parseTimeM)
import           Text.Megaparsec      (Parsec, eof, many, noneOf, oneOf, parse,
                                       parseMaybe, some, (<|>))
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

betragParser :: BcgeParser MonetaryValue
betragParser = do
  unitsString <- some (oneOf "-0123456789")
  units :: Integer <- return $ read unitsString
  char '.'
  centsString :: String <- some $ oneOf "0123456789"
  cents :: Integer <- return $ read centsString
  return $ fromUnitsAndCents units cents

-- Parsed Strings → Safe data (BCGE transactions)

-- | BCGE's transaction data fetched from their website.
data BcgeTransaction = BcgeTransaction{
  bTrDate     :: Day
  , bTrTitle  :: String
  , bTrAmount :: MonetaryValue
} deriving (Eq, Show)

data BcgeStatement = BcgeStatement{
  bStatementBalance        :: String -- TODO MonetaryValue
  , bStatementTransactions :: [BcgeTransaction]
} deriving (Eq, Show)

headerLine :: CsvLine
headerLine = ("Datum", "Buchungstext", "Betrag")

splitCsv :: [CsvLine] -> (Header, Content)
splitCsv csvLines = (take headerPos csvLines, drop (headerPos + 1) csvLines)
  where
    Just headerPos = elemIndex headerLine csvLines

getSaldo :: [CsvLine] -> String
getSaldo header = head [f0 | (f0, _, _) <- header, take 5 f0 == "Saldo"]

csvLineToBcgeTransaction :: CsvLine → Maybe BcgeTransaction
csvLineToBcgeTransaction (dateString, title, amountString) = do
  date <- parseTimeM True defaultTimeLocale "%d.%m.%y" dateString
  amount <- parseMaybe betragParser amountString
  return $ BcgeTransaction date title amount

csvLinesToBcgeStatement :: [CsvLine] -> BcgeStatement
csvLinesToBcgeStatement csvLines = BcgeStatement (getSaldo header) bcgeStatements
  where
    (header, content) = splitCsv csvLines
    Just bcgeStatements = traverse csvLineToBcgeTransaction content

bcgeCsvToLedger :: String → String
bcgeCsvToLedger input =
  let
    Right lines = parse bcgeCsvParser "" input
  in show . csvLinesToBcgeStatement $ lines
