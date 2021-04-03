{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module turns a parsed CS CSV statement into ledger format.
module Hledupt.CharlesSchwab.Ledger (
  csvToLedger,
) where

import qualified Control.Lens as L
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hledger (
  AmountPrice (UnitPrice),
  Status (Cleared, Pending),
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
import Hledupt.Data.Currency (Currency (USD))
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Relude (
  Applicative ((<*>)),
  Either (Right),
  Eq ((==)),
  Integer,
  Maybe (Just),
  Monad (return),
  Num (fromInteger),
  Text,
  ToString (toString),
  asum,
  guard,
  reverse,
  ($),
  (&),
  (++),
  (.),
  (>=>),
 )

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
    [ post (unvestedStockAccount symbol) (makeCommodityAmount symbol (fromInteger $ - q))
    , post (vestedStockAccount symbol) (makeCommodityAmount symbol (fromInteger q))
    ]
    & L.set tDescription (toString $ symbol `Text.append` " Vesting")
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
          & L.set pMaybeAmount (Just $ makeCurrencyAmount USD amount)
      , post "Expenses:Financial Services" missingamt
          & L.set pMaybeAmount (Just $ makeCurrencyAmount USD fee)
      ]
      & L.set tDescription (toString symbol ++ " Sale")
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
          ( makeCurrencyAmount USD (- amount)
          )
      , post "Taxes" missingamt
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
