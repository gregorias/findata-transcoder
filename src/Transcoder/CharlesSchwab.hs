-- | This module parses CharlesSchwab CSV statements.
module Transcoder.CharlesSchwab (
  parseBrokerageAccountHistory,
  parseEacAccountHistory,
) where

import Data.ByteString.Lazy qualified as LBS
import Hledger (Transaction)
import Relude
import Transcoder.CharlesSchwab.Brokerage.Csv qualified as BCsv
import Transcoder.CharlesSchwab.Eac.Csv qualified as ECsv
import Transcoder.CharlesSchwab.Eac.Ledger (eacHistoryToLedger)
import Transcoder.CharlesSchwab.Ledger qualified as Ledger

-- | Parses CSV history statement from a brokerage account.
parseBrokerageAccountHistory :: LBS.ByteString -> Either Text [Transaction]
parseBrokerageAccountHistory stmt = do
  recs <- BCsv.parseCsStatement stmt
  Ledger.brokerageHistoryToLedger recs

-- | Parses CSV history statement from an EAC account.
parseEacAccountHistory :: Text -> Either Text [Transaction]
parseEacAccountHistory stmt = do
  recordSheet <- ECsv.parseHistory stmt
  eacHistoryToLedger (ECsv.rsRecords recordSheet)
