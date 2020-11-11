{-# LANGUAGE TemplateHaskell #-}

module Mbank (Transaction, serializeTransactionsToLedgerDat) where

import Control.Lens (makeLenses, (^.), over)
import Data.Decimal (Decimal)

import Data.Time.Calendar (Day)

data Currency = USD
              | EUR
              | CHF
              | PLN
                  deriving (Eq, Show)

data Value = Value{ _amount :: Decimal, _currency :: Currency}
               deriving (Eq, Show)
makeLenses ''Value

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

serializeTransactionToLedger :: Transaction -> String
serializeTransactionToLedger = undefined

serializeTransactionsToLedgerDat :: [Transaction] -> String
serializeTransactionsToLedgerDat trs
  = header ++ concat (fmap serializeTransactionToLedger trs)

  where header = "; -*- ledger -*-\n; vim:filetype=ledger\n\n"

normalizeMbankTransaction :: MbankTransaction -> Transaction
normalizeMbankTransaction mTr = Transaction (mbankTransactionDate mTr) undefined [] undefined
