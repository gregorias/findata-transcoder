-- | This module parses a pdf2txt text dump of a Google Payslip.
module Transcoder.GPayslip.Pdf2Txt (
  payslipP,
) where

import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Relude
import Text.Megaparsec (
  Parsec,
  choice,
  count,
  manyTill,
  manyTill_,
  match,
  skipMany,
  try,
 )
import Text.Megaparsec.Char (
  char,
  digitChar,
  newline,
  space1,
  string,
 )
import Text.Megaparsec.Char.Extra (anyLineP)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (
    OptionalUnlimitedDecimalFraction,
    TwoDigitDecimalFraction
  ),
  decimalP,
 )
import Transcoder.GPayslip.Data (Deductions (..), Payslip (..))

dateLineP :: Parsec Void Text Day
dateLineP = do
  (dateString, _) <-
    try $
      match
        ( count 2 digitChar
            >> char '.'
            >> count 2 digitChar
            >> char '.'
            >> many digitChar
        )
  void newline
  parseTimeM
    False
    defaultTimeLocale
    "%d.%m.%Y"
    (toString dateString)

nameAndAmountLineP :: Text -> Parsec Void Text Decimal
nameAndAmountLineP name = do
  try $ void $ string name
  space1
  amount <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void newline
  return amount

deductionNetAmountP :: Parsec Void Text Decimal
deductionNetAmountP = nameAndAmountLineP "Deduction Net Amount"

mssbCsWithholdingP :: Parsec Void Text Decimal
mssbCsWithholdingP = nameAndAmountLineP "MSSB/CS Withholding"

ggiveP :: Parsec Void Text Decimal
ggiveP = nameAndAmountLineP "G Give charitable donation"

gcardRepaymentP :: Parsec Void Text Decimal
gcardRepaymentP = nameAndAmountLineP "Gcard Repayment"

pensionFundP :: Parsec Void Text Decimal
pensionFundP = nameAndAmountLineP "Pension Fund"

subTotalP :: Parsec Void Text Decimal
subTotalP = nameAndAmountLineP "Total"

-- | Parses a payslip line that in addition to an amount contains the reference and rate.
-- Returns the amount.
nameReferenceRateAmountLineP :: Parsec Void Text () -> Parsec Void Text Decimal
nameReferenceRateAmountLineP nameP = do
  try nameP
  space1
  _rate <- decimalP (DecimalFormat NoChunkSep (Just OptionalUnlimitedDecimalFraction))
  space1
  amount <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  _reference <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void newline
  return amount

salaryLineP :: Parsec Void Text ()
salaryLineP = do
  void $ try $ string "SALARY"
  void $ string " ELEMENTS AmountRate (%)\n"

deductionsLineP :: Parsec Void Text ()
deductionsLineP = do
  void $ try $ string "DEDUCTIONS"
  void $ string " Reference Rate (%)\n"

socialSecurityP :: Parsec Void Text Decimal
socialSecurityP =
  nameReferenceRateAmountLineP
    (void $ string "Swiss Social Security (AHV/IV/EO)")

unemploymentInsuranceP :: Parsec Void Text Decimal
unemploymentInsuranceP = do
  contribution0 <- nameReferenceRateAmountLineP unemploymentInsuranceNameP
  maybeContribution1 <- optional . try $ do
    void $ optional newline
    nameReferenceRateAmountLineP unemploymentInsuranceNameP
  return $ contribution0 + fromMaybe 0 maybeContribution1
 where
  unemploymentInsuranceNameP :: Parsec Void Text ()
  unemploymentInsuranceNameP = do
    void $
      choice
        [ string "Unemployment Insurance compl."
        , string "Unemployment Insurance"
        ]

taxAtSourceP :: Parsec Void Text Decimal
taxAtSourceP = do
  tax <- taxLineP
  taxs <- many $
    try $ do
      void $ optional newline
      taxLineP
  return $ tax + sum taxs
 where
  taxLineP = nameReferenceRateAmountLineP (void $ string "Tax at Source")

mainTotalP :: Parsec Void Text Decimal
mainTotalP = do
  try $ void $ string "TOTAL"
  space1
  total <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void $ string "CHF\n"
  return total

deductionsP :: Parsec Void Text Deductions
deductionsP = do
  deductionsLineP
  (_, socialSecurity) <- manyTill_ newline socialSecurityP
  skipMany newline
  unemploymentInsurance <- unemploymentInsuranceP
  skipMany newline
  pensionFund <- optional pensionFundP
  skipMany newline
  taxAtSource <- taxAtSourceP
  skipMany newline
  deductionNetAmount <- optional deductionNetAmountP
  skipMany newline
  mssbCsWithholding <- optional mssbCsWithholdingP
  skipMany newline
  ggive <- optional ggiveP
  skipMany newline
  gcard <- optional gcardRepaymentP
  (_, total) <- manyTill_ newline subTotalP
  return $
    Deductions
      { deductionsSwissSocialSecurity = socialSecurity
      , deductionsUnemploymentInsurance = unemploymentInsurance
      , deductionsPensionFund = pensionFund
      , deductionsTaxAtSource = taxAtSource
      , deductionsDeductionNetAmount = deductionNetAmount
      , deductionsMssbCsWithholding = mssbCsWithholding
      , deductionsGgive = ggive
      , deductionsGcard = gcard
      , deductionsTotal = total
      }

payslipP :: Parsec Void Text Payslip
payslipP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  void $ manyTill anyLineP salaryLineP
  (_, salaryTotal) <- manyTill_ anyLineP subTotalP
  void $ many newline
  deductions <- deductionsP
  (_, mainTotal) <- manyTill_ anyLineP mainTotalP
  void $ many anyLineP
  return $ Payslip day salaryTotal deductions mainTotal
