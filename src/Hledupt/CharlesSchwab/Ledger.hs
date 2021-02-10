{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module turns a parsed CS CSV statement into ledger format.
module Hledupt.CharlesSchwab.Ledger (
  csvToLedger,
) where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hledger (Status (Cleared, Pending), Transaction, missingamt, post, transaction)
import Hledger.Data (nulltransaction)
import Hledger.Data.Extra (makeCommodityAmount, makeCurrencyAmount)
import Hledger.Data.Lens (pMaybeAmount, pStatus, tDescription, tStatus)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord, csAmount, csDate, csQuantity, csSymbol),
  DollarAmount (..),
  csAction,
 )
import qualified Hledupt.CharlesSchwab.Csv as CsCsv
import Hledupt.Data.Currency (Currency (USD))
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Relude

-- "Wire Fund", "Sell" & "Journal", "Credit Interest"

usdAccount :: Text
usdAccount = "Assets:Liquid:Charles Schwab:USD"

unvestedStockAccount :: Text -> Text
unvestedStockAccount symbol = "Assets:Illiquid:Charles Schwab:Unvested " `Text.append` symbol

vestedStockAccount :: Text -> Text
vestedStockAccount symbol = "Assets:Investments:Charles Schwab:" `Text.append` symbol

data WireTransaction = WireTransaction
  { wireTransactionDate :: !Day
  , wireTransactionAmount :: !DollarAmount
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
          . L.set pMaybeAmount (Just $ makeCurrencyAmount USD amount)
    , post "TODO" missingamt
        & L.set pStatus Pending
    ]
    & L.set tDescription (toString wireFundsAction)

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
          & L.set pMaybeAmount (Just $ makeCurrencyAmount USD amount)
      , post "Income:Google" missingamt
      ]
      & L.set tDescription (toString $ csAction rec)
        . L.set tStatus Cleared

data Vesting = Vesting
  { vestingDate :: !Day
  , vestingSymbol :: !Text
  , vestingAmount :: !Integer
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
    [ post (unvestedStockAccount symbol) (makeCommodityAmount (toString symbol) (fromInteger $ - q))
    , post (vestedStockAccount symbol) (makeCommodityAmount (toString symbol) (fromInteger q))
    ]
    & L.set tDescription (toString $ symbol `Text.append` " Vesting")
      . L.set tStatus Cleared

csvRecordToLedgerTransaction :: CsCsvRecord -> Maybe Transaction
csvRecordToLedgerTransaction rec =
  let (wireF :: CsCsvRecord -> Maybe Transaction) = csvRecordToWireTransaction >=> (return . wireTransactionToLedgerTransaction)
      vestingF = csvRecordToVesting >=> (return . vestingToLedgerTransaction)
   in asum ([wireF, vestingF, creditInterestToLedgerTransaction] <*> [rec])

csvToLedger :: Vector CsCsvRecord -> Either Text LedgerReport
csvToLedger recs =
  let trs = Vector.mapMaybe csvRecordToLedgerTransaction recs
   in Right $ LedgerReport (Vector.toList trs) []
