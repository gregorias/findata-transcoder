-- | This module parses CharlesSchwab CSV statements.
module Transcoder.CharlesSchwab (
  parseBrokerageAccountHistory,
  parseEacAccountHistory,
) where

import Data.Aeson (Object, eitherDecodeStrict)
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
parseBrokerageAccountHistory = fmap BLedger.brokerageHistoryToLedger . BCsv.parseBrokerageHistoryCsv

-- | Parses a JSON history statement from an EAC account.
parseEacAccountHistory :: ByteString -> Either Text [Transaction]
parseEacAccountHistory stmt = do
  -- An assertion that provides a helpful message to the caller if they
  -- mistakenly provide something like a CSV file.
  assertJsonObject stmt
  recordSheet <- EacJson.parseHistory stmt
  eacHistoryToLedger (Eac.rsRecords recordSheet)
 where
  -- \| Asserts that the given bytestring is a JSON object.
  assertJsonObject :: ByteString -> Either Text ()
  assertJsonObject bs =
    either
      ( \msg ->
          Left
            $ "Could not decode the input as a valid JSON object."
            <> " EAC account history needs to be in the JSON format.\n"
            <> toText msg
      )
      (const $ Right ())
      (eitherDecodeStrict bs :: Either String Object)
