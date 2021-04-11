{-# LANGUAGE OverloadedStrings #-}

module Hledupt.Coop (
  receiptToLedger,
) where

import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

receiptToLedger :: Text -> Either Text LedgerReport
receiptToLedger = const $ Left "Unimplemented"
