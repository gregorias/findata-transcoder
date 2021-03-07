{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | This module parses a text dump from a Google Payslip and outputs a ledger.
module Hledupt.GPayslip (
  payslipTextToLedger,
  Payslip (..),
  SalaryElements (..),
  Deductions (..),
  PayslipLedgerConfig (..),
  parsePayslip,
  payslipToTransaction,
) where

import Control.Lens ((%~))
import qualified Control.Lens as L
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (Status (Cleared, Pending), Transaction, missingamt, post, transaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pMaybeAmount, pStatus, tDescription)
import Hledupt.Data.Currency (Currency (CHF))
import Hledupt.Data.LedgerReport (LedgerReport (..))
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

payslipToTransaction :: PayslipLedgerConfig -> Payslip -> Transaction
payslipToTransaction
  (PayslipLedgerConfig bankAccount _secondPillarAccount)
  (Payslip day mainTotal) =
    transaction
      day
      [ post bankAccount missingamt
          & L.set pStatus Pending
            . L.set pMaybeAmount (Just $ makeCurrencyAmount CHF mainTotal)
      , post "Income:Google" missingamt
          & L.set pStatus Cleared
            . L.set pMaybeAmount (Just $ makeCurrencyAmount CHF (- mainTotal))
      ]
      & L.set tDescription "Google Salary"

payslipToLedger :: PayslipLedgerConfig -> Payslip -> LedgerReport
payslipToLedger payslipLedgerConfig payslip =
  LedgerReport
    [payslipToTransaction payslipLedgerConfig payslip]
    []

-- | Transforms text extracted from a Google payslip's PDF into a
-- 'LedgerReport'.
payslipTextToLedger :: Text -> Either Text LedgerReport
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  return $ payslipToLedger defaultPayslipLedgerConfig payslip
