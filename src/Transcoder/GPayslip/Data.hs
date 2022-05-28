-- | Main data structures representing a Google Payslip.
module Transcoder.GPayslip.Data (
  Payslip (..),
  Deductions (..),
) where

import Data.Decimal (Decimal)
import Data.Time (Day)
import Relude

data Payslip = Payslip
  { payslipDate :: !Day
  , payslipMonthlySalaryTotal :: !Decimal
  , payslipDeductions :: !Deductions
  , payslipTotal :: !Decimal
  }
  deriving stock (Show, Eq)

data Deductions = Deductions
  { deductionsSwissSocialSecurity :: !Decimal
  , deductionsUnemploymentInsurance :: !Decimal
  , deductionsPensionFund :: !(Maybe Decimal)
  , deductionsTaxAtSource :: !Decimal
  , deductionsDeductionNetAmount :: !(Maybe Decimal)
  , deductionsMssbCsWithholding :: !(Maybe Decimal)
  , deductionsGgive :: !(Maybe Decimal)
  , deductionsGcard :: !(Maybe Decimal)
  , deductionsTotal :: !Decimal
  }
  deriving stock (Show, Eq)
