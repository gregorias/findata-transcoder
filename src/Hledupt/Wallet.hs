{-# LANGUAGE OverloadedStrings #-}

-- | This module contains data relevant to how I organize my Ledger file.
module Hledupt.Wallet (
  financialServices,
) where

import Hledger (AccountName)

financialServices :: AccountName
financialServices = "Expenses:Financial Services"
