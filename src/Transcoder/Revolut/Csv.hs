-- | Parses Revolut CSV files without (much) interpretation.
module Transcoder.Revolut.Csv (
  -- * Parsing
  parse,

  -- * Types
  Transaction (..),
  CompletedTransaction (..),
  RevertedTransaction (..),
  TransactionType (..),
) where

import Data.ByteString.Lazy qualified as LBS
import Data.Csv (parseNamedRecord)
import Data.Csv qualified as Csv
import Data.Csv.Extra (decodeByName', prependContextOnFailure, showNamedRecord)
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Relude
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import Transcoder.Data.Currency (Currency)
import Transcoder.Data.MyDecimal (unMyDecimal)

data TransactionType = Topup | CardPayment | Transfer | Exchange
  deriving stock (Show, Eq)

instance Csv.FromField TransactionType where
  parseField "TOPUP" = pure Topup
  parseField "CARD_PAYMENT" = pure CardPayment
  parseField "TRANSFER" = pure Transfer
  parseField "EXCHANGE" = pure Exchange
  parseField field =
    fail
      $ "Could not parse the field as a Revolut transaction type: "
      <> decodeUtf8 field
      <> "."

data TransactionState = Completed | Reverted
  deriving stock (Show, Eq)

instance Csv.FromField TransactionState where
  parseField "COMPLETED" = pure Completed
  parseField "REVERTED" = pure Reverted
  parseField field =
    fail
      $ "Could not parse the field as a Revolut transaction state: "
      <> decodeUtf8 field
      <> "."

newtype RevolutDay = RevolutDay
  { unRevolutDay :: Day
  }

instance Csv.FromField RevolutDay where
  parseField field = RevolutDay <$> parseTime "%Y-%m-%d %-k:%M:%S"
   where
    parseTime fmt = parseTimeM True defaultTimeLocale fmt (decodeUtf8 field)

data CompletedTransaction = CompletedTransaction
  { ctransactionType :: !TransactionType
  , ctStartedDate :: !Day
  , ctCompletedDate :: !Day
  , ctDescription :: !Text
  , ctAmount :: !Decimal
  , ctFee :: !Decimal
  , ctCurrency :: !Currency
  , ctBalance :: !Decimal
  }
  deriving stock (Show, Eq)

instance Csv.FromNamedRecord CompletedTransaction where
  parseNamedRecord nr = prependContextOnFailure
    ("Couldn't parse " <> toString (showNamedRecord nr) <> "\n")
    $ do
      trType <- Csv.lookup nr "Type"
      startedDate <- unRevolutDay <$> Csv.lookup nr "Started Date"
      completedDate <- unRevolutDay <$> Csv.lookup nr "Completed Date"
      description <- Csv.lookup nr "Description"
      amount <- unMyDecimal <$> Csv.lookup nr "Amount"
      fee <- unMyDecimal <$> Csv.lookup nr "Fee"
      currency <- Csv.lookup nr "Currency"
      balance <- unMyDecimal <$> Csv.lookup nr "Balance"
      return
        $ CompletedTransaction
          trType
          startedDate
          completedDate
          description
          amount
          fee
          currency
          balance

data RevertedTransaction = RevertedTransaction
  { rtDate :: !Day
  , rtDescription :: !Text
  , rtAmount :: !Decimal
  , rtFee :: !Decimal
  , rtCurrency :: !Currency
  }
  deriving stock (Show, Eq)

instance Csv.FromNamedRecord RevertedTransaction where
  parseNamedRecord nr = prependContextOnFailure
    ("Couldn't parse " <> toString (showNamedRecord nr) <> "\n")
    $ do
      date <- unRevolutDay <$> Csv.lookup nr "Started Date"
      description <- Csv.lookup nr "Description"
      amount <- unMyDecimal <$> Csv.lookup nr "Amount"
      fee <- unMyDecimal <$> Csv.lookup nr "Fee"
      currency <- Csv.lookup nr "Currency"
      return
        $ RevertedTransaction
          { rtDate = date
          , rtDescription = description
          , rtAmount = amount
          , rtFee = fee
          , rtCurrency = currency
          }

data Transaction
  = TransactionCompletedTransaction CompletedTransaction
  | TransactionRevertedTransaction RevertedTransaction
  deriving stock (Show, Eq)

instance Csv.FromNamedRecord Transaction where
  parseNamedRecord nr = do
    trState <- Csv.lookup nr "State"
    case trState of
      Completed -> TransactionCompletedTransaction <$> parseNamedRecord @CompletedTransaction nr
      Reverted -> TransactionRevertedTransaction <$> parseNamedRecord @RevertedTransaction nr

-- | Parses a Revolut CSV statement.
--
-- The left return value contains an error message.
parse :: CsvFile LBS.ByteString -> Either Text [Transaction]
parse (CsvFile csv) = decodeByName' csv
