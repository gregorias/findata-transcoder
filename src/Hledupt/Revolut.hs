module Hledupt.Revolut (
  parseCsvToLedger,
) where

import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

parseCsvToLedger :: Text -> Either Text LedgerReport
parseCsvToLedger = const $ Left "Unimplemented."
