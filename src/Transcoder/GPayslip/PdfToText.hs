-- | This module parses a pdftotext text dump of a Google Payslip.
module Transcoder.GPayslip.PdfToText (
  payslipP,
  Payslip (..),
  Deductions (..),
) where

import Data.Decimal (Decimal)
import Data.Time (Day)
import Data.Time.Extra (dayP)
import Relude
import Text.Megaparsec (Parsec, choice, skipMany, skipManyTill)
import Text.Megaparsec.Char (newline, space, space1, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  decimalP,
 )

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

type Parser = Parsec Void Text

cashAmountP :: Parser Decimal
cashAmountP = decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

dateLineP :: Parser Day
dateLineP = do
  void $ string "Date of payment"
  space
  dayP "%d.%m.%Y" <* newline

nameAndAmountLineP :: Text -> Parser Decimal
nameAndAmountLineP name = do
  void $ string name
  space1
  amount <- cashAmountP
  void newline
  return amount

-- | Parses a payslip line that in addition to an amount contains the reference and rate.
-- Returns the amount.
nameReferenceRateAmountLineP :: Parser a -> Parser Decimal
nameReferenceRateAmountLineP nameP = do
  void nameP
  space1
  _reference <- cashAmountP
  space1
  _rate <- decimalP (DecimalFormat NoChunkSep (Just OptionalUnlimitedDecimalFraction))
  space1
  amount <- cashAmountP
  void newline
  return amount

subTotalP :: Parser Decimal
subTotalP = nameAndAmountLineP "Total"

salaryP :: Parser Decimal
salaryP = do
  void $ string "SALARY ELEMENTS" >> anyLineP
  skipManyTill anyLineP subTotalP

unemploymentInsuranceP :: Parser Decimal
unemploymentInsuranceP = do
  contribution0 <- nameReferenceRateAmountLineP unemploymentInsuranceNameP
  maybeContribution1 <- optional $ do
    void $ optional newline
    nameReferenceRateAmountLineP unemploymentInsuranceNameP
  return $ contribution0 + fromMaybe 0 maybeContribution1
 where
  unemploymentInsuranceNameP = do
    choice
      [ string "Unemployment Insurance compl."
      , string "Unemployment Insurance"
      ]

taxAtSourceP :: Parser Decimal
taxAtSourceP = do
  tax <- some taxLineP
  return $ sum tax
 where
  taxLineP = nameReferenceRateAmountLineP (string "Tax at Source")

deductionsP :: Parser Deductions
deductionsP = do
  void $ string "DEDUCTIONS" >> anyLineP
  void newline
  ss <- nameReferenceRateAmountLineP (string "Swiss Social Security (AHV/IV/EO)")
  unemploymentInsurance <- unemploymentInsuranceP
  pensionFund <- optional $ nameAndAmountLineP "Pension Fund"
  taxAtSource <- taxAtSourceP
  deductionNetAmount <- optional $ nameAndAmountLineP "Deduction Net Amount"
  mssbCsWithholding <- optional $ nameAndAmountLineP "MSSB/CS Withholding"
  ggive <- optional $ nameAndAmountLineP "G Give charitable donation"
  gcard <- optional $ nameAndAmountLineP "Gcard Repayment"
  void newline
  total <- subTotalP
  return $
    Deductions
      { deductionsSwissSocialSecurity = ss
      , deductionsUnemploymentInsurance = unemploymentInsurance
      , deductionsPensionFund = pensionFund
      , deductionsTaxAtSource = taxAtSource
      , deductionsDeductionNetAmount = deductionNetAmount
      , deductionsMssbCsWithholding = mssbCsWithholding
      , deductionsGgive = ggive
      , deductionsGcard = gcard
      , deductionsTotal = total
      }

netIncomeP :: Parser Decimal
netIncomeP = do
  void $ string "NET INCOME" >> newline
  skipMany newline
  string "TOTAL" >> space1 >> string "CHF" >> space1
  cashAmountP <* newline

payslipP :: Parser Payslip
payslipP = do
  day <- skipManyTill anyLineP dateLineP
  skipMany newline
  salary <- salaryP
  skipMany newline
  deductions <- deductionsP
  netIncome <- skipManyTill anyLineP netIncomeP
  return $ Payslip day salary deductions netIncome
