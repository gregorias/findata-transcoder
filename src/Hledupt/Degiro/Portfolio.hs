{-# LANGUAGE OverloadedStrings #-}

-- | This module parses Degiro's Portfolio CSV statement into Ledger.
module Hledupt.Degiro.Portfolio (
  csvStatementToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
csvStatementToLedger = const $ Left "Hledupt.Degiro.Portfolio is unimplemented"
