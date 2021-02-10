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
import Data.Time (Day)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Hledger (Status (Cleared, Pending), Transaction, missingamt, post, transaction)
import Hledger.Data (nulltransaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pMaybeAmount, pStatus, tDescription, tStatus)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord, csAmount, csDate),
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

csvToLedger :: Vector CsCsvRecord -> Either Text LedgerReport
csvToLedger recs =
  let wireTrs = wireTransactionToLedgerTransaction <$> Vector.mapMaybe csvRecordToWireTransaction recs
   in Right $ LedgerReport (Vector.toList wireTrs) []
