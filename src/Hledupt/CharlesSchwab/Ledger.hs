{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | This module turns a parsed CS CSV statement into ledger format.
module Hledupt.CharlesSchwab.Ledger (
  csvToLedger,
  parseStatement,
) where

import qualified Data.ByteString.Lazy as LBS
import Data.Vector (Vector)
import Hledupt.CharlesSchwab.Csv (CsCsvRecord (CsCsvRecord))
import qualified Hledupt.CharlesSchwab.Csv as CsCsv
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Relude

-- "Wire Fund", "Sell" & "Journal", "Credit Interest"

csvToLedger :: Vector CsCsvRecord -> Either Text LedgerReport
csvToLedger recs = Left "Unimplemented"

parseStatement :: LBS.ByteString -> Either Text LedgerReport
parseStatement stmt = do
  recs <- CsCsv.parseCsStatement stmt
  csvToLedger recs
