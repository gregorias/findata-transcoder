module Hledupt.Revolut (
  parseCsvToLedger,
) where

import Control.Lens (over, _Left)
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledupt.Data.Currency (Currency)
import Hledupt.Data.LedgerReport (LedgerReport)
import Hledupt.Data.MyDecimal (MyDecimal (unMyDecimal))
import Relude

data TransactionType = Topup | CardPayment | Transfer | Exchange

data OneWayTransactionType = OwttTopup | OwttCardPayment | OwttTransfer

instance Csv.FromField TransactionType where
  parseField "TOPUP" = pure Topup
  parseField "CARD_PAYMENT" = pure CardPayment
  parseField "TRANSFER" = pure Transfer
  parseField "EXCHANGE" = pure Exchange
  parseField field = fail $ "Could not parse transaction type: " <> decodeUtf8 field

instance Csv.FromField OneWayTransactionType where
  parseField "TOPUP" = pure OwttTopup
  parseField "CARD_PAYMENT" = pure OwttCardPayment
  parseField "TRANSFER" = pure OwttTransfer
  parseField field = fail $ "Could not parse transaction type: " <> decodeUtf8 field

data OneWayTransaction = OneWayTransaction
  { transactionType :: !OneWayTransactionType
  , date :: !Day
  , description :: !Text
  , amount :: !Decimal
  , currency :: !Currency
  , balance :: !Decimal
  }

newtype RevolutDay = RevolutDay
  { unRevolutDay :: Day
  }

instance Csv.FromField RevolutDay where
  parseField field =
    RevolutDay
      <$> parseTimeM True defaultTimeLocale "%Y-%m-%d %T" (decodeUtf8 field)

instance Csv.FromNamedRecord OneWayTransaction where
  parseNamedRecord nr = do
    trType <- Csv.lookup nr "Type"
    startedDate <- unRevolutDay <$> Csv.lookup nr "Started Date"
    description <- Csv.lookup nr "Description"
    amount <- unMyDecimal <$> Csv.lookup nr "Amount"
    currency <- Csv.lookup nr "Currency"
    balance <- unMyDecimal <$> Csv.lookup nr "Balance"
    return $
      OneWayTransaction
        trType
        startedDate
        description
        amount
        currency
        balance

decodeCsv :: LByteString -> Either Text [OneWayTransaction]
decodeCsv csv = toList . snd <$> over _Left toText (Csv.decodeByName csv)

parseCsvToLedger :: LByteString -> Either Text LedgerReport
parseCsvToLedger csv = do
  csvTransactions <- decodeCsv csv
  Left "Processing read CSV data is unimplemented."
