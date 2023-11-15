-- | This module contains data relevant to how I organize my Ledger file.
module Transcoder.Wallet (
  (<:>),

  -- * Accounts
  assets,
  liquidAssets,
  investmentAssets,
  bcgeAccount,
  bcgeCCAccount,
  degiroAccount,
  revolutAccount,
  debtAssets,
  equity,
  expenses,
  expensesOther,
  expensesTransport,
  expensesYtPremium,
  financialServices,
) where

import Hledger (AccountName)
import Relude

assets :: AccountName
assets = "Assets"

equity :: AccountName
equity = "Equity"

(<:>) :: AccountName -> AccountName -> AccountName
(<:>) parent child = parent <> ":" <> child

-- Assign fixity 5, which is one less than for '<>' (6) and therefore binds
-- less tightly.
infixl 5 <:>

liquidAssets :: AccountName
liquidAssets = assets <:> "Liquid"

investmentAssets :: AccountName
investmentAssets = assets <:> "Investments"

debtAssets :: AccountName
debtAssets = assets <:> "Debts"

bcgeAccount :: AccountName
bcgeAccount = liquidAssets <:> "BCGE"

bcgeCCAccount :: AccountName
bcgeCCAccount = liquidAssets <:> "BCGE CC"

degiroAccount :: AccountName
degiroAccount = liquidAssets <:> "Degiro"

revolutAccount :: AccountName
revolutAccount = liquidAssets <:> "Revolut"

expenses :: AccountName
expenses = "Expenses"

financialServices :: AccountName
financialServices = expenses <:> "Financial Services"

expensesOther :: AccountName
expensesOther = expenses <:> "Other"

expensesTransport :: AccountName
expensesTransport = expenses <:> "Transport"

expensesYtPremium :: AccountName
expensesYtPremium = expenses <:> "Leisure" <:> "YouTube"
