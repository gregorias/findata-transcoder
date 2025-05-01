module Transcoder.Revolut (
  parseCsvToLedger,
) where

import Control.Lens (over, _Left)
import Control.Lens qualified as L
import Data.Csv qualified as Csv
import Data.Csv.Extra (prependContextOnFailure, showNamedRecord)
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (AccountName, Status (Cleared, Pending), Transaction, balassert, missingamt)
import Hledger.Data (post, transaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pBalanceAssertion, pStatus, tDescription, tStatus)
import Relude
import Transcoder.Data.Currency (Currency)
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))
import Transcoder.Data.MyDecimal (MyDecimal (unMyDecimal))
import Transcoder.Wallet (liquidAssets, (<:>))

data TransactionType = Topup | CardPayment | Transfer | Exchange

getAccountName :: Currency -> AccountName
getAccountName currency = liquidAssets <:> "Revolut" <:> show currency

instance Csv.FromField TransactionType where
  parseField "TOPUP" = pure Topup
  parseField "CARD_PAYMENT" = pure CardPayment
  parseField "TRANSFER" = pure Transfer
  parseField "EXCHANGE" = pure Exchange
  parseField field = fail $ "Could not parse transaction type: " <> decodeUtf8 field

data RevolutTransaction = RevolutTransaction
  { _transactionType :: !TransactionType
  , _date :: !Day
  , _description :: !Text
  , _amount :: !Decimal
  , _currency :: !Currency
  , _balance :: !Decimal
  }

newtype RevolutDay = RevolutDay
  { unRevolutDay :: Day
  }

instance Csv.FromField RevolutDay where
  parseField field = RevolutDay <$> (parseTime "%Y-%m-%d %T" <|> parseTime "%Y-%m-%d %-k:%M:%S")
   where
    parseTime fmt = parseTimeM True defaultTimeLocale fmt (decodeUtf8 field)

instance Csv.FromNamedRecord RevolutTransaction where
  parseNamedRecord nr = prependContextOnFailure
    ("Couldn't parse " <> toString (showNamedRecord nr) <> "\n")
    $ do
      trType <- Csv.lookup nr "Type"
      startedDate <- unRevolutDay <$> Csv.lookup nr "Started Date"
      description <- Csv.lookup nr "Description"
      amount <- unMyDecimal <$> Csv.lookup nr "Amount"
      currency <- Csv.lookup nr "Currency"
      balance <- unMyDecimal <$> Csv.lookup nr "Balance"
      return
        $ RevolutTransaction
          trType
          startedDate
          description
          amount
          currency
          balance

decodeCsv :: LByteString -> Either Text [RevolutTransaction]
decodeCsv csv = toList . snd <$> over _Left toText (Csv.decodeByName csv)

revolutTransactionToLedgerTransaction :: RevolutTransaction -> Transaction
revolutTransactionToLedgerTransaction (RevolutTransaction _type date description amount currency balance) =
  transaction
    date
    [ post (getAccountName currency) (makeCurrencyAmount currency amount)
        & L.set
          pBalanceAssertion
          (balassert . makeCurrencyAmount currency $ balance)
    , post "Todo" missingamt
        & L.set pStatus Pending
    ]
    & L.set tDescription description
    . L.set tStatus Cleared

parseCsvToLedger :: LByteString -> Either Text LedgerReport
parseCsvToLedger csv = do
  csvTransactions <- decodeCsv csv
  let ledgerTransactions = revolutTransactionToLedgerTransaction <$> csvTransactions
  return $ LedgerReport ledgerTransactions []
