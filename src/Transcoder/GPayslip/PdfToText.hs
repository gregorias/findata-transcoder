{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module parses a pdftotext text dump of a Google Payslip.
module Transcoder.GPayslip.PdfToText (
  -- * Data types
  Payslip (..),
  Item (..),
  PayslipSummary (..),
  Earnings (..),
  NotionalPay (..),
  StatutoryDeductions (..),
  OtherPaymentsAndDeductions (..),
  PaymentDetails (..),

  -- * The parser
  payslipP,
) where

import Control.Monad.Permutations (runPermutation, toPermutation, toPermutationWithDefault)
import Data.Char
import Data.Decimal (Decimal)
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  decimalP,
 )
import Data.Time (Day, fromGregorian)
import Data.Time.Calendar.Extra
import Relude
import Text.Megaparsec (Parsec, anySingle, label, satisfy, single, skipManyTill, try)
import Text.Megaparsec.Char (char, newline, space1, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Char.Lexer

-- | A single item (row) in a payslip.
data Item = Item
  { itemPriorPeriod :: !Decimal
  , itemCurrent :: !Decimal
  }
  deriving stock (Show, Eq)

-- | The Payslip data as represented on the PDF.
data Payslip = Payslip
  { payslipDay :: !Day
  -- ^ The day of payment.
  , payslipSummary :: !PayslipSummary
  , payslipEarnings :: !Earnings
  , payslipNotionalPay :: !NotionalPay
  , payslipStatutoryDeductions :: !StatutoryDeductions
  , payslipOtherPaymentsAndDeductions :: !OtherPaymentsAndDeductions
  , payslipNetPay :: !Decimal
  , payslipPaymentDetails :: !PaymentDetails
  }
  deriving stock (Show, Eq)

-- | The summary equation of the payslip.
data PayslipSummary = PayslipSummary
  { payslipSummaryEarnings :: !Decimal
  , payslipSummaryStatutoryDeductions :: !Decimal
  , payslipSummaryOtherPaymentsAndDeductions :: !Decimal
  , payslipSummaryNetPay :: !Decimal
  }
  deriving stock (Show, Eq)

-- | All items under the earnings section.
data Earnings = Earnings
  { earningsMonthlySalary :: !Item
  , earningsErHealthInsurance :: !Item
  , earningsAnnualBonusGross :: !(Maybe Item)
  , earningsBonusGross :: !(Maybe Item)
  , earningsSpotBonusGross :: !(Maybe Item)
  , earningsPeerBonus :: !(Maybe Item)
  , earningsEducationSubsidyGross :: !(Maybe Item)
  , earningsNonCashSpotBonusGrossUp :: !(Maybe Item)
  , earningsMealAllowanceGrossUp :: !(Maybe Item)
  , earningsTotal :: !Item
  }
  deriving stock (Show, Eq)

-- | All notional pay items.
data NotionalPay = NotionalPay
  { notionalPayNonCashSpotBonus :: !(Maybe Item)
  , notionalPayMealAllowanceNet :: !(Maybe Item)
  , notionalPayGsuGainIncomeTaxes :: !(Maybe Item)
  , notionalPayGsuGainSocialSecurity :: !(Maybe Item)
  , notionalPayTotal :: !Item
  }
  deriving stock (Show, Eq)

-- | All statutory deductions.
data StatutoryDeductions = StatutoryDeductions
  { statutoryDeductionsWht :: !Item
  , statutoryDeductionsSwissSocialSecurity :: !Item
  , statutoryDeductionsUnemploymentInsurance :: !Item
  , statutoryDeductionsTotal :: !Item
  }
  deriving stock (Show, Eq)

-- | All items under the "Other Payments and Deductions" section.
data OtherPaymentsAndDeductions = OtherPaymentsAndDeductions
  { otherPaymentsAndDeductionsPensionContributionEe :: !Item
  , otherPaymentsAndDeductionsMssbWitholdingCredit :: !(Maybe Item)
  , otherPaymentsAndDeductionsGcardRepayment :: !(Maybe Item)
  , otherPaymentsAndDeductionsGgiveDeductions :: !(Maybe Item)
  , otherPaymentsAndDeductionsTotal :: !Item
  }
  deriving stock (Show, Eq)

-- | Payment details provided on the payslip.
data PaymentDetails = PaymentDetails
  { paymentDetailsBankAccountIban :: !Text
  , paymentDetailsBankName :: !Text
  , paymentDetailsAmount :: !Decimal
  }
  deriving stock (Show, Eq)

type Parser = Parsec Void Text

-- Parses a decimal amount, e.g., "1,234.56".
cashAmountP :: Parser Decimal
cashAmountP = decimalP (DecimalFormat (ChunkSep ',') (Just TwoDigitDecimalFraction))

-- Parses a decimal amount with CHF at the end, e.g., "1,234.56 CHF".
cashAmountWithChfP :: Parser Decimal
cashAmountWithChfP = cashAmountP <* (single ' ' >> string "CHF")

itemP ::
  -- | The name of the item
  Text ->
  Parser Item
itemP name = do
  void $ string name
  space1
  priorPeriod <- cashAmountP
  space1
  current <- cashAmountP
  void newline
  return $ Item priorPeriod current

-- | Parses a payslip date, e.g., "24 March 2023".
dateP :: Parser Day
dateP = label "date" $ do
  day <- decimal
  void $ char ' '
  month <- monthP
  void $ char ' '
  year <- decimal
  return $ fromGregorian year month day

-- | Parses the payslip's header and extracts the date of payment.
payslipHeaderP :: Parser Day
payslipHeaderP = label "payslip header" $ do
  void $ skipManyTill anySingle (string "Date of payment: ")
  dateP <* newline

payslipSummaryP :: Parser PayslipSummary
payslipSummaryP = label "payslip summary" $ do
  void $ try (space1 >> string "Payslip summary" >> newline)
  replicateM_ 5 anyLineP
  earnings <- void space1 *> cashAmountP <* space1
  deductions <- cashAmountP <* space1
  otherPaymentsAndDeductions <- cashAmountP <* space1
  netPay <- cashAmountP <* newline
  return
    PayslipSummary
      { payslipSummaryEarnings = earnings
      , payslipSummaryStatutoryDeductions = deductions
      , payslipSummaryOtherPaymentsAndDeductions = otherPaymentsAndDeductions
      , payslipSummaryNetPay = netPay
      }

earningsP :: Parser Earnings
earningsP = label "earnings" $ do
  void $ try (space1 >> string "Earnings" >> newline)
  void
    $ string "Taxable Earnings"
    >> space1
    >> string "Earning type"
    >> space1
    >> string "Prior Period"
    >> space1
    >> string "Unit"
    >> space1
    >> string "Rate of Pay"
    >> space1
    >> string "Current"
    >> newline
  monthlySalary <- earningsItemP "Monthly Salary"
  erHealthInsurance <- earningsItemP "ER Health Insurance"
  annualBonusGross <- optional $ earningsItemP "Annual Bonus Gross"
  bonusGross <- optional $ earningsItemP "Bonus Gross"
  spotBonusGross <- optional $ earningsItemP "Spot Bonus Gross"
  peerBonus <- optional $ earningsItemP "Peer Bonus"
  educationSubsidyGross <- optional $ earningsItemP "Education Subsidy Gross"
  nonCashSpotBonusGrossUp <- optional $ earningsItemP "NonCash Spot Bonus GrosUp"
  mealAllowanceGrossUp <- optional $ earningsItemP "Meal Allowance Gross Up"
  void $ itemP "Total Taxable Earnings"
  total <- itemP "Total Earnings"
  return
    $ Earnings
      { earningsMonthlySalary = monthlySalary
      , earningsErHealthInsurance = erHealthInsurance
      , earningsAnnualBonusGross = annualBonusGross
      , earningsBonusGross = bonusGross
      , earningsSpotBonusGross = spotBonusGross
      , earningsPeerBonus = peerBonus
      , earningsEducationSubsidyGross = educationSubsidyGross
      , earningsNonCashSpotBonusGrossUp = nonCashSpotBonusGrossUp
      , earningsMealAllowanceGrossUp = mealAllowanceGrossUp
      , earningsTotal = total
      }
 where
  earningsItemP ::
    Text ->
    Parser Item
  earningsItemP name = do
    void $ string name >> space1 >> string "Cash" >> space1
    priorPeriod <- cashAmountP
    space1
    current <- cashAmountP
    void newline
    return $ Item priorPeriod current

notionalPayP :: Parser NotionalPay
notionalPayP = label "notional pay" $ do
  void $ string "Notional Pay" >> space1 >> string "Earning type" >> space1 >> string "Prior period" >> anyLineP
  nonCashSpotBonus <- optional $ do
    void $ string "Non Cash Spot Bonus" >> space1 >> string "BIK" >> space1
    priorPeriod <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return Item{itemPriorPeriod = priorPeriod, itemCurrent = current}
  mealAllowanceNet <- optional $ do
    void $ string "Meal Allowance Net" >> space1 >> string "BIK" >> space1
    priorPeriod <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return Item{itemPriorPeriod = priorPeriod, itemCurrent = current}
  gsuGainIncomeTaxes <- optional $ do
    void $ string "GSU gain (Income Taxes)" >> space1 >> string "GSU" >> space1
    priorPeriod <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return Item{itemPriorPeriod = priorPeriod, itemCurrent = current}
  gsuGainSocialSecurity <- optional $ do
    void $ string "GSU gain (Social Secur.)" >> space1 >> string "GSU" >> space1
    priorPeriod <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return Item{itemPriorPeriod = priorPeriod, itemCurrent = current}
  total <- do
    void $ string "Total Notional Pay" >> space1
    priorPeriod <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return Item{itemPriorPeriod = priorPeriod, itemCurrent = current}
  return
    $ NotionalPay
      { notionalPayNonCashSpotBonus = nonCashSpotBonus
      , notionalPayMealAllowanceNet = mealAllowanceNet
      , notionalPayGsuGainIncomeTaxes = gsuGainIncomeTaxes
      , notionalPayGsuGainSocialSecurity = gsuGainSocialSecurity
      , notionalPayTotal = total
      }

statutoryDeductionsP :: Parser StatutoryDeductions
statutoryDeductionsP = label "statutory deductions" $ do
  void $ try (space1 >> string "Statutory Deductions" >> newline)
  void
    $ string "Statutory Deductions:"
    >> space1
    >> string "Prior Period"
    >> space1
    >> string "Current rate %"
    >> space1
    >> string "Current"
    >> newline
  wht <- itemWithCurrentRateP "WHT"
  swissSocialSecurity <- itemWithCurrentRateP "Swiss Social Security"
  unemploymentInsurance <- itemWithCurrentRateP "Unemployment Insurance"
  total <- itemP "Total Statutory Deductions"
  return
    $ StatutoryDeductions
      { statutoryDeductionsWht = wht
      , statutoryDeductionsSwissSocialSecurity = swissSocialSecurity
      , statutoryDeductionsUnemploymentInsurance = unemploymentInsurance
      , statutoryDeductionsTotal = total
      }
 where
  itemWithCurrentRateP ::
    -- \| The name of the item
    Text ->
    Parser Item
  itemWithCurrentRateP name = do
    void $ string name >> space1
    priorPeriod <- cashAmountP <* space1
    _currentRate <- cashAmountP <* space1
    current <- cashAmountP <* newline
    return $ Item priorPeriod current

otherPaymentsAndDeductionsP :: Parser OtherPaymentsAndDeductions
otherPaymentsAndDeductionsP = label "other payments and deductions" $ do
  void $ try (space1 >> string "Other Payments and Deductions" >> newline)
  void
    $ string "Other Payments and Deductions"
    >> space1
    >> string "Prior Period"
    >> space1
    >> string "Current"
    >> newline
  ( gGiveDeductions
    , pensionContributionEe
    , mssbWitholdingCredit
    , gcardRepayment
    ) <-
    runPermutation
      $ (,,,)
      <$> toPermutationWithDefault Nothing (Just <$> itemP "gGive Deductions")
      <*> toPermutation (itemP "Pension Contribution EE")
      <*> toPermutationWithDefault Nothing (Just <$> itemP "MSSB Withholding Credit")
      <*> toPermutationWithDefault Nothing (Just <$> itemP "Gcard Repayment")
  total <- itemP "Total Other Payments and Deductions"
  return $ OtherPaymentsAndDeductions pensionContributionEe mssbWitholdingCredit gcardRepayment gGiveDeductions total

netPayP :: Parser Decimal
netPayP = label "net pay" $ do
  void $ string "Net Pay"
  space1
  cashAmountWithChfP

paymentDetailsP :: Parser PaymentDetails
paymentDetailsP = label "payment details" $ do
  void $ try (space1 >> string "Payment Details" >> newline)
  void
    ( string "Payment Method"
        >> space1
        >> string "Bank name"
        >> space1
        >> string "IBAN"
        >> space1
        >> string "Amount"
        >> newline
    )
  void $ string "Payroll ACH" >> space1
  bankName <- string "Banque Cantonale de Geneve"
  void space1
  -- Get a string of ascii characters representing IBAN without whitespace.
  bankIban <- label "IBAN" $ toText <$> some (satisfy (\x -> isAscii x && not (isSpace x)))
  amount <- label "cash amount" $ space1 >> cashAmountP
  void newline
  return $ PaymentDetails bankIban bankName amount

{-# HLINT ignore "Use <$>" #-}

-- | Parses a payslip PDF that was run through `pdftotext -layout`.
payslipP :: Parser Payslip
payslipP = do
  day <- payslipHeaderP
  summary <- payslipSummaryP
  earnings <- earningsP
  notionalPay <- notionalPayP
  statutoryDeductions <- skipManyTill newline statutoryDeductionsP
  otherPaymentsAndDeductions <- skipManyTill newline otherPaymentsAndDeductionsP
  netPay <- skipManyTill newline netPayP
  paymentDetails <- skipManyTill newline paymentDetailsP
  void $ many anyLineP
  return $ Payslip day summary earnings notionalPay statutoryDeductions otherPaymentsAndDeductions netPay paymentDetails
