-- | This module turns a parsed brokerage account history CSV statement into ledger format.
module Transcoder.CharlesSchwab.Brokerage.Ledger (
  brokerageHistoryToLedger,
) where

import Control.Lens qualified as L
import Data.Cash (Cash (Cash))
import Data.Decimal (Decimal)
import Data.Time (Day)
import Hledger (
  AccountName,
  AmountPrice (UnitPrice),
  Status (Cleared, Pending),
  Transaction,
  amountSetFullPrecision,
  missingamt,
  post,
  transaction,
 )
import Hledger.Data.Extra (
  Comment (NoComment),
  ToAmount (toAmount),
  makeCommodityAmount,
  makeCurrencyAmount,
  makePosting,
  makeTransaction,
 )
import Hledger.Data.Lens (aAmountPrice, pMaybeAmount, pStatus, tDescription, tStatus)
import Relude
import Transcoder.CharlesSchwab.Brokerage.Csv (
  BrokerageHistoryCsvRecord (bhcrAction, bhcrAmount, bhcrDate, bhcrDescription, bhcrFees, bhcrPrice, bhcrQuantity, bhcrSymbol),
  bhcrAction,
 )
import Transcoder.CharlesSchwab.DollarAmount (
  DollarAmount (..),
 )
import Transcoder.Data.Currency (usd)
import Transcoder.Data.LedgerReport (todoPosting)
import Transcoder.Wallet (
  equity,
  (<:>),
 )

-- "Wire Fund", "Sell" & "Journal", "Credit Interest"

equityCs :: AccountName
equityCs = equity <:> "Charles Schwab"

usdAccount :: Text
usdAccount = "Assets:Liquid:Charles Schwab:USD"

unvestedGoog :: AccountName
unvestedGoog = equityCs <:> "Unvested GOOG"

vestedStockAccount :: Text -> Text
vestedStockAccount symbol = "Assets:Investments:Charles Schwab:" <> symbol

data WireTransaction = WireTransaction
  { _wireTransactionDate :: !Day
  , _wireTransactionAmount :: !DollarAmount
  }

wireFundsAction :: Text
wireFundsAction = "Wire Funds"

csvRecordToWireTransaction :: BrokerageHistoryCsvRecord -> Maybe WireTransaction
csvRecordToWireTransaction rec = do
  guard $ bhcrAction rec == wireFundsAction
  amount <- bhcrAmount rec
  return $ WireTransaction (bhcrDate rec) amount

wireTransactionToLedgerTransaction :: WireTransaction -> Transaction
wireTransactionToLedgerTransaction (WireTransaction day (DollarAmount amount)) =
  transaction
    day
    [ post usdAccount missingamt
        & L.set pStatus Cleared
        . L.set pMaybeAmount (Just $ makeCurrencyAmount usd amount)
    , todoPosting
    ]
    & L.set tDescription wireFundsAction

creditInterestAction :: Text
creditInterestAction = "Credit Interest"

creditInterestToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
creditInterestToLedgerTransaction rec = do
  guard $ bhcrAction rec == creditInterestAction
  (DollarAmount amount) <- bhcrAmount rec
  return
    $ transaction
      (bhcrDate rec)
      [ post usdAccount missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount usd amount)
      , post "Income:Google" missingamt
      ]
    & L.set tDescription (bhcrAction rec)
    . L.set tStatus Cleared

data Vesting = Vesting
  { _vestingDate :: !Day
  , _vestingSymbol :: !Text
  , _vestingAmount :: !Decimal
  }

vestingAction :: Text
vestingAction = "Stock Plan Activity"

csvRecordToVesting :: BrokerageHistoryCsvRecord -> Maybe Vesting
csvRecordToVesting rec = do
  guard $ bhcrAction rec == vestingAction
  quantity <- bhcrQuantity rec
  return $ Vesting (bhcrDate rec) (bhcrSymbol rec) quantity

vestingToLedgerTransaction :: Vesting -> Transaction
vestingToLedgerTransaction (Vesting day symbol q) =
  transaction
    day
    [ post unvestedGoog (makeCommodityAmount symbol (-q))
    , post (vestedStockAccount symbol) (makeCommodityAmount symbol q)
    ]
    & L.set tDescription (symbol <> " Vesting")
    . L.set tStatus Cleared

wireSentToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
wireSentToLedgerTransaction rec = do
  guard $ bhcrAction rec == "Wire Sent"
  (DollarAmount amount) <- bhcrAmount rec
  return
    $ makeTransaction
      (bhcrDate rec)
      Nothing
      (bhcrAction rec)
      [ makePosting
          Cleared
          usdAccount
          (toAmount $ Cash usd amount)
          NoComment
      , makePosting Pending "ToDo" Nothing NoComment
      ]

saleToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
saleToLedgerTransaction rec = do
  guard $ bhcrAction rec == "Sell"
  (DollarAmount amount) <- bhcrAmount rec
  (DollarAmount fee) <- bhcrFees rec
  (DollarAmount price) <- bhcrPrice rec
  q <- bhcrQuantity rec
  let symbol = bhcrSymbol rec
  return
    $ transaction
      (bhcrDate rec)
      [ post
          (vestedStockAccount symbol)
          ( makeCommodityAmount symbol (-q)
              & L.set
                aAmountPrice
                ( Just
                    . UnitPrice
                    $ makeCommodityAmount "USD" price
                    & amountSetFullPrecision
                )
          )
      , post usdAccount missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount usd amount)
      , post "Expenses:Financial Services" missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount usd fee)
      ]
    & L.set tDescription (symbol <> " Sale")
    . L.set tStatus Cleared

taxToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
taxToLedgerTransaction rec = do
  guard $ bhcrAction rec == "Journal"
  guard $ bhcrDescription rec == "Gencash transaction for SPS RS Lapse Tool"
  (DollarAmount amount) <- bhcrAmount rec
  return
    $ transaction
      (bhcrDate rec)
      [ post
          usdAccount
          (makeCurrencyAmount usd amount)
      , post (equityCs <:> "Unvested GOOG Withholding Tax") missingamt
      ]
    & L.set tDescription "Withholding Tax"
    . L.set tStatus Cleared

csvRecordToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
csvRecordToLedgerTransaction rec =
  let (wireF :: BrokerageHistoryCsvRecord -> Maybe Transaction) = csvRecordToWireTransaction >=> (return . wireTransactionToLedgerTransaction)
      vestingF = csvRecordToVesting >=> (return . vestingToLedgerTransaction)
   in asum
        ( [ wireF
          , vestingF
          , creditInterestToLedgerTransaction
          , saleToLedgerTransaction
          , taxToLedgerTransaction
          , wireSentToLedgerTransaction
          ]
            <*> [rec]
        )

-- | Transcodes a parsed brokerage account history CSV statement into ledger format.
brokerageHistoryToLedger :: [BrokerageHistoryCsvRecord] -> [Transaction]
brokerageHistoryToLedger recs = reverse $ mapMaybe csvRecordToLedgerTransaction recs
