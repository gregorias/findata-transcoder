-- | This module turns a parsed CS CSV statement into ledger format.
module Transcoder.CharlesSchwab.Ledger (
  csvToLedger,
) where

import Control.Lens qualified as L
import Data.Cash (Cash (Cash))
import Data.Time (Day)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
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
import Hledger.Data.Extra (Comment (NoComment), makeCommodityAmount, makeCurrencyAmount, makePosting, makeTransaction)
import Hledger.Data.Lens (aAmountPrice, pMaybeAmount, pStatus, tDescription, tStatus)
import Relude
import Transcoder.CharlesSchwab.Csv (
  CsCsvRecord (csAmount, csDate, csDescription, csFees, csPrice, csQuantity, csSymbol),
  DollarAmount (..),
  csAction,
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

csvRecordToWireTransaction :: CsCsvRecord -> Maybe WireTransaction
csvRecordToWireTransaction rec = do
  guard $ csAction rec == wireFundsAction
  amount <- csAmount rec
  return $ WireTransaction (csDate rec) amount

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

creditInterestToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
creditInterestToLedgerTransaction rec = do
  guard $ csAction rec == creditInterestAction
  (DollarAmount amount) <- csAmount rec
  return $
    transaction
      (csDate rec)
      [ post usdAccount missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount usd amount)
      , post "Income:Google" missingamt
      ]
      & L.set tDescription (csAction rec)
        . L.set tStatus Cleared

data Vesting = Vesting
  { _vestingDate :: !Day
  , _vestingSymbol :: !Text
  , _vestingAmount :: !Integer
  }

vestingAction :: Text
vestingAction = "Stock Plan Activity"

csvRecordToVesting :: CsCsvRecord -> Maybe Vesting
csvRecordToVesting rec = do
  guard $ csAction rec == vestingAction
  quantity <- csQuantity rec
  return $ Vesting (csDate rec) (csSymbol rec) quantity

vestingToLedgerTransaction :: Vesting -> Transaction
vestingToLedgerTransaction (Vesting day symbol q) =
  transaction
    day
    [ post unvestedGoog (makeCommodityAmount symbol (fromInteger $ -q))
    , post (vestedStockAccount symbol) (makeCommodityAmount symbol (fromInteger q))
    ]
    & L.set tDescription (symbol <> " Vesting")
      . L.set tStatus Cleared

wireSentToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
wireSentToLedgerTransaction rec = do
  guard $ csAction rec == "Wire Sent"
  (DollarAmount amount) <- csAmount rec
  return $
    makeTransaction
      (csDate rec)
      Nothing
      (csAction rec)
      [ makePosting
          (Just Cleared)
          usdAccount
          (Just $ Cash usd amount)
          NoComment
      , makePosting (Just Pending) "ToDo" Nothing NoComment
      ]

saleToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
saleToLedgerTransaction rec = do
  guard $ csAction rec == "Sell"
  (DollarAmount amount) <- csAmount rec
  (DollarAmount fee) <- csFees rec
  (DollarAmount price) <- csPrice rec
  q <- csQuantity rec
  let symbol = csSymbol rec
  return $
    transaction
      (csDate rec)
      [ post
          (vestedStockAccount symbol)
          ( makeCommodityAmount symbol (fromInteger $ -q)
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

taxToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
taxToLedgerTransaction rec = do
  guard $ csAction rec == "Journal"
  guard $ csDescription rec == "Gencash transaction for SPS RS Lapse Tool"
  (DollarAmount amount) <- csAmount rec
  return $
    transaction
      (csDate rec)
      [ post
          usdAccount
          ( makeCurrencyAmount usd amount
          )
      , post (equityCs <:> "Unvested GOOG Withholding Tax") missingamt
      ]
      & L.set tDescription "Withholding Tax"
        . L.set tStatus Cleared

csvRecordToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
csvRecordToLedgerTransaction rec =
  let (wireF :: CsCsvRecord -> Maybe Transaction) = csvRecordToWireTransaction >=> (return . wireTransactionToLedgerTransaction)
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

csvToLedger :: Vector CsCsvRecord -> Either Text [Transaction]
csvToLedger recs = do
  let trs = Vector.mapMaybe csvRecordToLedgerTransaction recs
  return $ reverse $ Vector.toList trs
