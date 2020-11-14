{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mbank (
  MbankTransaction(..),
  valueParser,
  fromZloteAndGrosze,
  mbankCsvParser,
  parseMbankCsv,
  serializeTransactionsToLedgerDat) where

import Control.Lens (makeLenses, (^.), over)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Ratio ((%))

import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import Text.Megaparsec (Parsec, eof, parse, many, (<|>), oneOf, noneOf)
import Text.Megaparsec.Char (string, newline, char)

data Currency = USD
              | EUR
              | CHF
              | PLN
                  deriving (Eq, Show)

data Value = Value{ _amount :: Decimal, _currency :: Currency}
               deriving (Eq)
makeLenses ''Value
instance Show Value where
  show (Value amount currency) = show amount ++ " " ++ show currency

negateValue v = over amount negate v

addValue (Value a0 c0) (Value a1 c1)
  | c0 == c1 = Just $ Value (a0 + a1) c0
  | otherwise = Nothing

data Posting = Posting{account :: String, value :: Value, balance :: Value,
                       exchangeRate :: String}

data Transaction = Transaction{date :: Day, description :: String,
                               postings :: [Posting], comment :: String}

-- | A type representing Mbank's monetary value, i.e., a decimal with 2 decimal
-- places
type MonetaryValue = Decimal


fromZloteAndGrosze :: (Integral a) => a -> a -> MonetaryValue
fromZloteAndGrosze zlote grosze = zloteDec + groszeDec
  where
    zloteDec = realFracToDecimal 0 (zlote % 1)
    groszeDec = realFracToDecimal 2 (grosze % 100)


-- | mBank's transaction data fetched from their website.
data MbankTransaction = MbankTransaction{mbankTransactionDate :: Day,
                                         mbankTransactionTitle :: String,
                                         mbankTransactionAmount :: MonetaryValue,
                                         mbankTransactionEndBalance :: MonetaryValue}
                                         deriving (Eq, Show)



serializeTransactionToLedger :: Transaction -> String
serializeTransactionToLedger = undefined

serializeTransactionsToLedgerDat :: [Transaction] -> String
serializeTransactionsToLedgerDat trs
  = headerParser ++ concat (fmap serializeTransactionToLedger trs)

  where headerParser = "; -*- ledger -*-\n; vim:filetype=ledger\n\n"

normalizeMbankTransaction :: MbankTransaction -> Transaction
normalizeMbankTransaction mTr = Transaction (mbankTransactionDate mTr) undefined [] undefined

type MbankParser = Parsec () String

headerParser :: MbankParser ()
headerParser = string "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n" >> return ()

dateParser :: MbankParser Day
dateParser = do
  dateString <- many (oneOf "-0123456789")
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

valueParser :: MbankParser Decimal
valueParser = do
  zloteString <- removeSpaces <$> many (oneOf "-0123456789 ")
  zlote :: Integer <- return $ read zloteString
  groszeString <- (char ',' *> many (oneOf "0123456789") <* char ' ') <|> pure "00"
  grosze :: Integer <- return $ read groszeString
  string "PLN"
  return $ fromZloteAndGrosze zlote grosze
  where
    removeSpaces = filter ((/=) ' ')

mbankCsvTransactionParser :: MbankParser MbankTransaction
mbankCsvTransactionParser = do
  date <- (dateParser <* char ';')
  title <- (char '"' *> many (noneOf "\";") <* char '"' <* char ';')
  (many (noneOf ";") >> char ';')
  (many (noneOf ";") >> char ';')
  value <- (valueParser <* char ';')
  balance <- (valueParser <* char ';')
  ((newline >> return ()) <|> eof)
  return $ MbankTransaction date title value balance

mbankCsvParser :: MbankParser [MbankTransaction]
mbankCsvParser = do
  headerParser
  many mbankCsvTransactionParser <* eof

parseMbankCsv :: String -> String
parseMbankCsv inputCsv =
  let transactions = parse mbankCsvParser "" inputCsv
  in show transactions
