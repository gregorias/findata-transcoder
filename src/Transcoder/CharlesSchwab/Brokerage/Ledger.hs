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
 )
import Hledger.Data.Extra (
  Comment (NoComment),
  ToAmount (toAmount),
  makeCommodityAmount,
  makeCurrencyAmount,
  makePosting,
  makeTransaction,
 )
import Hledger.Data.Lens (
  aAmountPrice,
 )
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
  makeTransaction
    day
    Nothing
    wireFundsAction
    [ makePosting Cleared usdAccount (makeCurrencyAmount usd amount) NoComment
    , todoPosting
    ]

creditInterestAction :: Text
creditInterestAction = "Credit Interest"

creditInterestToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
creditInterestToLedgerTransaction rec = do
  guard $ bhcrAction rec == creditInterestAction
  (DollarAmount amount) <- bhcrAmount rec
  return
    $ makeTransaction
      (bhcrDate rec)
      Cleared
      (bhcrAction rec)
      [ makePosting Nothing usdAccount (makeCurrencyAmount usd amount) NoComment
      , makePosting Nothing "Income:Google" missingamt NoComment
      ]

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
  makeTransaction
    day
    Cleared
    (symbol <> " Vesting")
    [ makePosting Nothing unvestedGoog (makeCommodityAmount symbol (-q)) NoComment
    , makePosting Nothing (vestedStockAccount symbol) (makeCommodityAmount symbol q) NoComment
    ]

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
    $ makeTransaction
      (bhcrDate rec)
      Cleared
      (symbol <> " Sale")
      [ makePosting
          Nothing
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
          NoComment
      , makePosting Nothing usdAccount (makeCurrencyAmount usd amount) NoComment
      , makePosting Nothing "Expenses:Financial Services" (makeCurrencyAmount usd fee) NoComment
      ]

taxToLedgerTransaction :: BrokerageHistoryCsvRecord -> Maybe Transaction
taxToLedgerTransaction rec = do
  guard $ bhcrAction rec == "Journal"
  guard $ bhcrDescription rec == "Gencash transaction for SPS RS Lapse Tool"
  (DollarAmount amount) <- bhcrAmount rec
  return
    $ makeTransaction
      (bhcrDate rec)
      Cleared
      "Withholding Tax"
      [ makePosting Nothing usdAccount (makeCurrencyAmount usd amount) NoComment
      , makePosting Nothing (equityCs <:> "Unvested GOOG Withholding Tax") missingamt NoComment
      ]

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
