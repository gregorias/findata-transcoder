{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (Transaction)
import Hledupt.Data.LedgerReport (LedgerReport)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Relude
import Text.Megaparsec (Parsec, anySingle, count, errorBundlePretty, manyTill, manyTill_, match, parse, try)
import Text.Megaparsec.Char (
  char,
  digitChar,
  newline,
  space1,
  string,
 )

data PayslipDuo = PayslipDuo
  { payslipDuoDate :: !Day
  , payslipDuoSalaryElements :: !SalaryElements
  , payslipDuoDeductions :: ()
  , payslipDuoTotal :: !Decimal
  }
  deriving stock (Show, Eq)

data Payslip = Payslip
  { payslipDate :: !Day
  , payslipTotal :: !Decimal
  }
  deriving stock (Show, Eq)

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

anyLineP :: Parsec Void Text ()
anyLineP = void $ manyTill anySingle newline

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
    (Text.unpack dateString)

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
  (_, mainTotal) <- manyTill_ anyLineP mainTotalP
  void $ many anyLineP
  return $ Payslip day mainTotal

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

payslipToTransaction :: Payslip -> Transaction
payslipToTransaction (Payslip _day _total) = undefined

payslipToLedger :: Payslip -> Either Text LedgerReport
payslipToLedger _payslip = Left "Unimplemented"

payslipTextToLedger :: Text -> Either Text LedgerReport
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  payslipToLedger payslip
