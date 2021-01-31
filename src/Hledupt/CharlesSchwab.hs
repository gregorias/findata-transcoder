{-# LANGUAGE OverloadedStrings #-}

-- | This module parses CharlesSchwab CSV statement
module Hledupt.CharlesSchwab (
  csvToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

csvToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
csvToLedger _ = Left "Unimplemented"
