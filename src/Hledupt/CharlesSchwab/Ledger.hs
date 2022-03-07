-- | This module turns a parsed CS CSV statement into ledger format.
module Hledupt.CharlesSchwab.Ledger (
  csvToLedger,
) where

import qualified Control.Lens as L
import Data.Time (Day)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hledger (
  AccountName,
  AmountPrice (UnitPrice),
  Status (Cleared),
  Transaction,
  amountSetFullPrecision,
  missingamt,
  post,
  transaction,
 )
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens (aAmountPrice, pMaybeAmount, pStatus, tDescription, tStatus)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (csAmount, csDate, csDescription, csFees, csPrice, csQuantity, csSymbol),
  DollarAmount (..),
  csAction,
 )
import Hledupt.Data.Currency (usd)
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport), todoPosting)
import Hledupt.Wallet (
  equity,
  (<:>),
 )
import Relude

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
    [ post unvestedGoog (makeCommodityAmount symbol (fromInteger $ - q))
    , post (vestedStockAccount symbol) (makeCommodityAmount symbol (fromInteger q))
    ]
    & L.set tDescription (symbol <> " Vesting")
      . L.set tStatus Cleared

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
          ( makeCommodityAmount symbol (fromInteger $ - q)
              & L.set
                aAmountPrice
                ( Just . UnitPrice $
                    makeCommodityAmount "USD" price
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
          ( makeCurrencyAmount usd (- amount)
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
          ]
            <*> [rec]
        )

csvToLedger :: Vector CsCsvRecord -> Either Text LedgerReport
csvToLedger recs =
  let trs = Vector.mapMaybe csvRecordToLedgerTransaction recs
   in Right $ LedgerReport (reverse $ Vector.toList trs) []
