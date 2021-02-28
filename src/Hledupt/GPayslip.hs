{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module parses a text dump from a Google Payslip and outputs a ledger.
module Hledupt.GPayslip (
  Payslip (..),
  SalaryElements (..),
  Deductions (..),
  parsePayslip,
  payslipTextToLedger,
) where

import Data.Decimal (Decimal)
import Data.Time (Day)
import Hledupt.Data.LedgerReport (LedgerReport)
import Relude

data Payslip = Payslip
  { payslipDate :: !Day
  , payslipSalaryElements :: !SalaryElements
  , payslipDeductions :: ()
  , payslipTotal :: !Decimal
  }

data SalaryElements = SalaryElements
  { salaryElementsMonthlySalary :: !Decimal
  , salaryElementsTotal :: !Decimal
  }

data Deductions = Deductions
  { deductionsSwissSocialSecurity :: !Decimal
  , deductionsUnemploymentInsurance :: !Decimal
  , deductionsPensionFund :: !Decimal
  , deductionsTaxAtSource :: !Decimal
  , deductionsTotal :: !Decimal
  }

parsePayslip :: Text -> Either Text Payslip
parsePayslip _payslip = Left "Unimplemented"

payslipToLedger :: Payslip -> Either Text LedgerReport
payslipToLedger _payslip = Left "Unimplemented"

payslipTextToLedger :: Text -> Either Text LedgerReport
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  payslipToLedger payslip
