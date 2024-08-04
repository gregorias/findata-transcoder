-- | This module parses CharlesSchwab CSV statements.
module Transcoder.CharlesSchwab (
  parseBrokerageAccountHistory,
  parseEacAccountHistory,
) where

import Data.ByteString.Lazy qualified as LBS
import Hledger (Transaction)
import Relude
import Transcoder.CharlesSchwab.Brokerage.Csv qualified as BCsv
import Transcoder.CharlesSchwab.Brokerage.Ledger qualified as BLedger
import Transcoder.CharlesSchwab.Eac.Data qualified as Eac
import Transcoder.CharlesSchwab.Eac.Json qualified as EacJson
import Transcoder.CharlesSchwab.Eac.Ledger (eacHistoryToLedger)

-- | Parses CSV history statement from a brokerage account.
parseBrokerageAccountHistory :: LBS.ByteString -> Either Text [Transaction]
parseBrokerageAccountHistory stmt = do
  recs <- BCsv.parseBrokerageHistoryCsv stmt
  BLedger.brokerageHistoryToLedger recs

-- | Parses a JSON history statement from an EAC account.
parseEacAccountHistory :: ByteString -> Either Text [Transaction]
parseEacAccountHistory stmt = do
  recordSheet <- EacJson.parseHistory stmt
  eacHistoryToLedger (Eac.rsRecords recordSheet)
