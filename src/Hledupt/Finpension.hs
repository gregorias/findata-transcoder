{-# LANGUAGE OverloadedStrings #-}

module Hledupt.Finpension (
  transactionsToLedger,
) where

import qualified Data.ByteString.Lazy as LBS
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

transactionsToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
transactionsToLedger = const . Left $ "unimplemented"
