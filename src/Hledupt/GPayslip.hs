{-# LANGUAGE OverloadedStrings #-}

-- | This module parses a text dump from a Google Payslip and outputs a ledger.
module Hledupt.GPayslip (
  parsePayslip,
) where

import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

parsePayslip :: Text -> Either Text LedgerReport
parsePayslip _payslip = Left "Unimplented"
