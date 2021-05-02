{-# LANGUAGE OverloadedStrings #-}

-- | This module contains data relevant to how I organize my Ledger file.
module Hledupt.Wallet (
  bcgeCCAccount,
  financialServices,
) where

import Hledger (AccountName)

bcgeCCAccount :: AccountName
bcgeCCAccount = "Assets:Liquid:BCGE CC"

financialServices :: AccountName
financialServices = "Expenses:Financial Services"
