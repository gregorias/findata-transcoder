{-# LANGUAGE OverloadedStrings #-}

-- | This module contains data relevant to how I organize my Ledger file.
module Hledupt.Wallet (
  bcgeAccount,
  bcgeCCAccount,
  expensesOther,
  expensesTransport,
  financialServices,
) where

import Hledger (AccountName)
import Relude

assets :: AccountName
assets = "Assets"

liquidAssets :: AccountName
liquidAssets = assets <> ":Liquid"

bcgeAccount :: AccountName
bcgeAccount = liquidAssets <> ":BCGE"

bcgeCCAccount :: AccountName
bcgeCCAccount = liquidAssets <> ":BCGE CC"

expenses :: AccountName
expenses = "Expenses"

financialServices :: AccountName
financialServices = expenses <> ":Financial Services"

expensesOther :: AccountName
expensesOther = expenses <> ":Other"

expensesTransport :: AccountName
expensesTransport = expenses <> ":Transport"
