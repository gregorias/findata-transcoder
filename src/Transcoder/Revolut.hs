module Transcoder.Revolut (
  parseCsvToLedger,
) where

import Control.Lens qualified as L
import Data.Decimal (Decimal)
import Data.Time (Day)
import Hledger (AccountName, Status (Cleared, Pending), Transaction, balassert, missingamt)
import Hledger.Data (post, transaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pBalanceAssertion, pStatus, tDescription, tStatus)
import Relude
import Transcoder.Data.CsvFile
import Transcoder.Data.Currency (Currency)
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))
import Transcoder.Revolut.Csv qualified as RevolutCsv
import Transcoder.Wallet (liquidAssets, (<:>))

getAccountName :: Currency -> AccountName
getAccountName currency = liquidAssets <:> "Revolut" <:> show currency

data RevolutTransaction = RevolutTransaction
  { _date :: !Day
  , _description :: !Text
  , _amount :: !Decimal
  , _currency :: !Currency
  , _balance :: !Decimal
  }

csvTransactionToRevolutTransaction :: RevolutCsv.Transaction -> Either Text RevolutTransaction
csvTransactionToRevolutTransaction
  ( RevolutCsv.TransactionCompletedTransaction
      ( RevolutCsv.CompletedTransaction
          _
          date
          _
          description
          amount
          _fee
          currency
          balance
        )
    ) =
    Right
      $ RevolutTransaction
        date
        description
        amount
        currency
        balance
csvTransactionToRevolutTransaction _ = Left "Unimplemented"

revolutTransactionToLedgerTransaction :: RevolutTransaction -> Transaction
revolutTransactionToLedgerTransaction (RevolutTransaction date description amount currency balance) =
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
  csvTransactions <- RevolutCsv.parse (CsvFile csv)
  revolutTransactions <- mapM csvTransactionToRevolutTransaction csvTransactions
  let ledgerTransactions = revolutTransactionToLedgerTransaction <$> revolutTransactions
  return $ LedgerReport ledgerTransactions []
