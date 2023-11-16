-- | This module parses CharlesSchwab CSV statements.
module Transcoder.CharlesSchwab (
  parseBrokerageAccountHistory,
) where

import Data.ByteString.Lazy qualified as LBS
import Hledger (Transaction)
import Relude
import Transcoder.CharlesSchwab.Brokerage.Csv qualified as BCsv
import Transcoder.CharlesSchwab.Ledger qualified as Ledger

parseBrokerageAccountHistory :: LBS.ByteString -> Either Text [Transaction]
parseBrokerageAccountHistory stmt = do
  recs <- BCsv.parseCsStatement stmt
  Ledger.brokerageHistoryToLedger recs
