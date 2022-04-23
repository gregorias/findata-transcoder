-- | This module parses a text dump from a Google Payslip and outputs a ledger.
module Transcoder.GPayslip (
  parsePayslip,
  payslipToLedger,
  payslipTextToLedger,
  Payslip (..),
  Deductions (..),
  PayslipLedgerConfig (..),
) where

import Control.Lens ((^.))
import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (toGregorian)
import Hledger (Status (Cleared, Pending), Transaction, missingamt, post, transaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pMaybeAmount, pStatus, tDescription)
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (
    OptionalUnlimitedDecimalFraction,
    TwoDigitDecimalFraction
  ),
  decimalP,
 )
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
import Text.Megaparsec.Extra (
  parsePretty,
 )

data PayslipLedgerConfig = PayslipLedgerConfig
  { -- | The bank account Google sends the salary to.
    payslipLedgerConfigBankAccount :: !Text
  , payslipLedgerConfigSecondPillarAccount :: !Text
  }
  deriving stock (Show, Eq)

defaultPayslipLedgerConfig :: PayslipLedgerConfig
defaultPayslipLedgerConfig =
  PayslipLedgerConfig
    { payslipLedgerConfigBankAccount = "Assets:Liquid:BCGE"
    , payslipLedgerConfigSecondPillarAccount =
        "Assets:Illiquid:AXA Wintherthur Pension Fund"
    }

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

subTotalP :: Parsec Void Text Decimal
subTotalP = nameAndAmountLineP "Total"

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

mainTotalP :: Parsec Void Text Decimal
mainTotalP = do
  try $ void $ string "TOTAL"
  space1
  total <- decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  void $ string "CHF\n"
  return total

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

parsePayslip :: Text -> Either Text Payslip
parsePayslip = parsePretty payslipP "the Google payslip"

payslipToLedger :: PayslipLedgerConfig -> Payslip -> Transaction
payslipToLedger
  (PayslipLedgerConfig bankAccount secondPillarAccount)
  ( Payslip
      day
      salaryTotal
      Deductions
        { deductionsSwissSocialSecurity = socialSecurity
        , deductionsUnemploymentInsurance = unemploymentInsurance
        , deductionsPensionFund = maybePensionFund
        , deductionsTaxAtSource = taxAtSource
        , deductionsDeductionNetAmount = maybeDeductionNetAmount
        , deductionsMssbCsWithholding = maybeMssbCsWithholding
        , deductionsGgive = maybeGgive
        , deductionsGcard = maybeGcard
        , deductionsTotal = _
        }
      mainTotal
    ) =
    transaction
      day
      ( [ post bankAccount missingamt
            & L.set pStatus Pending
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf mainTotal)
        , post "Income:Google" missingamt
            & L.set pStatus Cleared
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf (- salaryTotal))
        , post (statePrefix <> "Mandatory Contributions:Social Security") missingamt
            & L.set pStatus Cleared
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf socialSecurity)
        , post (statePrefix <> "Mandatory Contributions:Unemployment Insurance") missingamt
            & L.set pStatus Cleared
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf unemploymentInsurance)
        , post (statePrefix <> "Withholding Tax:Total") missingamt
            & L.set pStatus Cleared
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf taxAtSource)
        ]
          ++ maybe
            []
            ( \pensionFund ->
                [ post secondPillarAccount missingamt
                    & L.set pStatus Cleared
                      . L.set pMaybeAmount (Just $ makeCurrencyAmount chf pensionFund)
                ]
            )
            maybePensionFund
          ++ maybe
            []
            ( \deductionNetAmount ->
                [ post "Equity:Google Deduction Net Amount" missingamt
                    & L.set pStatus Cleared
                      . L.set pMaybeAmount (Just $ makeCurrencyAmount chf deductionNetAmount)
                ]
            )
            maybeDeductionNetAmount
          ++ maybe
            []
            ( \mssbCs ->
                [ post "Equity:MssbCs Withholding" missingamt
                    & L.set pStatus Cleared
                      . L.set pMaybeAmount (Just $ makeCurrencyAmount chf mssbCs)
                ]
            )
            maybeMssbCsWithholding
          ++ maybe
            []
            ( \ggive ->
                [ post "Expenses:Other" missingamt
                    & L.set pStatus Cleared
                      . L.set pMaybeAmount (Just $ makeCurrencyAmount chf ggive)
                ]
            )
            maybeGgive
          ++ maybe
            []
            ( \gcard ->
                [ post "Assets:Debts:Google" missingamt
                    & L.set pStatus Cleared
                      . L.set pMaybeAmount (Just $ makeCurrencyAmount chf gcard)
                ]
            )
            maybeGcard
      )
      & L.set tDescription "Google Salary"
   where
    year :: Integer = toGregorian day ^. L._1
    statePrefix = "State:" <> show year <> ":"

-- | Transforms text extracted from a Google payslip's PDF into a
-- 'LedgerReport'.
payslipTextToLedger :: Text -> Either Text Transaction
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  return $ payslipToLedger defaultPayslipLedgerConfig payslip
