-- | This module parses CharlesSchwab CSV statements.
module Transcoder.CharlesSchwab (
  parseBrokerageAccountHistory,
) where

import Data.ByteString.Lazy qualified as LBS
import Hledger (Transaction)
import Relude
import Transcoder.CharlesSchwab.Csv qualified as CsCsv
import Transcoder.CharlesSchwab.Ledger qualified as Ledger

parseBrokerageAccountHistory :: LBS.ByteString -> Either Text [Transaction]
parseBrokerageAccountHistory stmt = do
  recs <- CsCsv.parseCsStatement stmt
  Ledger.csvToLedger recs
