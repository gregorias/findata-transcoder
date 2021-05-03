{-# LANGUAGE OverloadedStrings #-}

-- | This module contains data relevant to how I organize my Ledger file.
module Hledupt.Wallet (
  bcgeCCAccount,
  expensesOther,
  financialServices,
) where

import Hledger (AccountName)
import Relude

bcgeCCAccount :: AccountName
bcgeCCAccount = "Assets:Liquid:BCGE CC"

expenses :: AccountName
expenses = "Expenses"

financialServices :: AccountName
financialServices = expenses <> ":Financial Services"

expensesOther :: AccountName
expensesOther = expenses <> ":Other"
