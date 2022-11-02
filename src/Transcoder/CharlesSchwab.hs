-- | This module parses CharlesSchwab CSV statement
module Transcoder.CharlesSchwab (
  csvToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import Relude
import qualified Transcoder.CharlesSchwab.Csv as CsCsv
import qualified Transcoder.CharlesSchwab.Ledger as Ledger
import Transcoder.Data.LedgerReport (LedgerReport)

csvToLedger :: LBS.ByteString -> Either Text LedgerReport
csvToLedger stmt = do
  recs <- CsCsv.parseCsStatement stmt
  Ledger.csvToLedger recs
