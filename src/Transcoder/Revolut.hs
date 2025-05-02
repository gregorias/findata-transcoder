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
  , _balance :: !(Maybe Decimal)
  }

csvTransactionToRevolutTransaction :: RevolutCsv.Transaction -> RevolutTransaction
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
    RevolutTransaction
      date
      description
      amount
      currency
      (Just balance)
csvTransactionToRevolutTransaction
  ( RevolutCsv.TransactionRevertedTransaction
      ( RevolutCsv.RevertedTransaction
          date
          description
          amount
          _fee
          currency
        )
    ) =
    RevolutTransaction
      date
      description
      (-amount)
      currency
      Nothing

revolutTransactionToLedgerTransaction :: RevolutTransaction -> Transaction
revolutTransactionToLedgerTransaction (RevolutTransaction date description amount currency balance) =
  transaction
    date
    [ revolutPostingWithoutBalance
        & maybe id addBalance balance
    , post "Todo" missingamt
        & L.set pStatus Pending
    ]
    & L.set tDescription description
    . L.set tStatus Cleared
 where
  revolutPostingWithoutBalance = post (getAccountName currency) (makeCurrencyAmount currency amount)
  addBalance balance = L.set pBalanceAssertion (balassert . makeCurrencyAmount currency $ balance)

parseCsvToLedger :: LByteString -> Either Text LedgerReport
parseCsvToLedger csv = do
  csvTransactions <- RevolutCsv.parse (CsvFile csv)
  let revolutTransactions = map csvTransactionToRevolutTransaction csvTransactions
  let ledgerTransactions = revolutTransactionToLedgerTransaction <$> revolutTransactions
  return $ LedgerReport ledgerTransactions []
