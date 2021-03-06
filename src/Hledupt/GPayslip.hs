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

import Control.Lens ((%~))
import qualified Control.Lens as L
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time (Day)
import Hledupt.Data.LedgerReport (LedgerReport)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Relude
import Text.Megaparsec (Parsec, anySingle, errorBundlePretty, manyTill, manyTill_, parse, try)
import Text.Megaparsec.Char (
  newline,
  space1,
  string,
 )

data PayslipDuo = PayslipDuo
  { payslipDate :: !Day
  , payslipSalaryElements :: !SalaryElements
  , payslipDeductions :: ()
  , payslipTotal :: !Decimal
  }
  deriving stock (Show, Eq)

newtype Payslip = Payslip Decimal
  deriving newtype (Show, Eq)

data SalaryElements = SalaryElements
  { salaryElementsMonthlySalary :: !Decimal
  , salaryElementsTotal :: !Decimal
  }
  deriving stock (Show, Eq)

data Deductions = Deductions
  { deductionsSwissSocialSecurity :: !Decimal
  , deductionsUnemploymentInsurance :: !Decimal
  , deductionsPensionFund :: !Decimal
  , deductionsTaxAtSource :: !Decimal
  , deductionsTotal :: !Decimal
  }
  deriving stock (Show, Eq)

anyLine :: Parsec Void Text ()
anyLine = void $ manyTill anySingle newline

mainTotalP :: Parsec Void Text Decimal
mainTotalP = do
  try $ void $ string "TOTAL"
  space1
  total <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void $ string "CHF\n"
  return total

payslipP :: Parsec Void Text Payslip
payslipP = do
  (_, mainTotal) <- manyTill_ anyLine mainTotalP
  void $ many anyLine
  return $ Payslip mainTotal

prependErrorMessage :: Text -> Either Text a -> Either Text a
prependErrorMessage err = L._Left %~ (errln `Text.append`)
 where
  errln = err `Text.append` "\n"

parsePayslip :: Text -> Either Text Payslip
parsePayslip payslip = prepareErrMsg parsedPayslip
 where
  parsedPayslip = parse payslipP "" payslip
  prepareErrMsg =
    prependErrorMessage "Could not parse the payslip."
      . first (Text.pack . errorBundlePretty)

payslipToLedger :: Payslip -> Either Text LedgerReport
payslipToLedger _payslip = Left "Unimplemented"

payslipTextToLedger :: Text -> Either Text LedgerReport
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  payslipToLedger payslip
