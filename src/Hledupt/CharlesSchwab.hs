{-# LANGUAGE OverloadedStrings #-}

-- | This module parses CharlesSchwab CSV statement
module Hledupt.CharlesSchwab (
  csvToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Hledupt.CharlesSchwab.Csv as CsCsv
import qualified Hledupt.CharlesSchwab.Ledger as Ledger
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

csvToLedger :: LBS.ByteString -> Either Text LedgerReport
csvToLedger stmt = do
  recs <- CsCsv.parseCsStatement stmt
  Ledger.csvToLedger recs
