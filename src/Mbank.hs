{-# LANGUAGE TemplateHaskell #-}

module Mbank (
  Transaction,
  parseMbankCsv,
  serializeTransactionsToLedgerDat) where

import Control.Lens (makeLenses, (^.), over)
import Data.Decimal (Decimal)

import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

import Text.Parsec (Parsec, eof, parse, many, (<|>))
import Text.Parsec.Char (string, newline, char, anyChar, alphaNum, oneOf, noneOf)

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

-- | mBank's transaction data fetched from their website.
data MbankTransaction = MbankTransaction{mbankTransactionDate :: Day,
                                         mbankTransactionTitle :: String,
                                         mbankTransactionAmount :: Value,
                                         mbankTransactionEndBalance :: Value}
                                         deriving (Show)

serializeTransactionToLedger :: Transaction -> String
serializeTransactionToLedger = undefined

serializeTransactionsToLedgerDat :: [Transaction] -> String
serializeTransactionsToLedgerDat trs
  = header ++ concat (fmap serializeTransactionToLedger trs)

  where header = "; -*- ledger -*-\n; vim:filetype=ledger\n\n"

normalizeMbankTransaction :: MbankTransaction -> Transaction
normalizeMbankTransaction mTr = Transaction (mbankTransactionDate mTr) undefined [] undefined

header :: Parsec String () ()
header = string "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n" >> return ()

parseDate :: Parsec String () Day
parseDate = do
  dateString <- many (oneOf "-0123456789")
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

parseValue :: Parsec String () Value
parseValue = do
  many (oneOf "-0123456789, ")
  string "PLN"
  return $ Value 0 PLN

mbankTransactionCsv :: Parsec String () MbankTransaction
mbankTransactionCsv = do
  date <- (parseDate <* char ';')
  title <- (char '"' *> many (noneOf "\";") <* char '"' <* char ';')
  (many (noneOf ";") >> char ';')
  (many (noneOf ";") >> char ';')
  value <- (parseValue <* char ';')
  balance <- (parseValue <* char ';')
  ((newline >> return ()) <|> eof)
  return $ MbankTransaction date title value balance

mbankCsv :: Parsec String () [MbankTransaction]
mbankCsv = do
  header
  many mbankTransactionCsv <* eof

parseMbankCsv :: String -> String
parseMbankCsv inputCsv =
  let transactions = parse mbankCsv "" inputCsv
  in show transactions
