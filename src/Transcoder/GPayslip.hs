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
import Data.Cash (Cash (Cash))
import Data.Decimal (Decimal)
import Data.Time.Calendar (toGregorian)
import Hledger (AccountName, Posting, Status (Cleared, Pending), Transaction, transaction)
import Hledger.Data.Extra (Comment (NoComment), makePosting)
import Hledger.Data.Lens (tDescription)
import Relude
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.GPayslip.Data (Deductions (..), Payslip (..))
import Transcoder.GPayslip.PdfToText (
  payslipP,
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
      ( [ makePosting (Just Pending) bankAccount (Just $ Cash chf mainTotal) NoComment
        , mkPosting "Income:Google" (-salaryTotal)
        , mkPosting (statePrefix <> "Mandatory Contributions:Social Security") socialSecurity
        , mkPosting (statePrefix <> "Mandatory Contributions:Unemployment Insurance") unemploymentInsurance
        , mkPosting (statePrefix <> "Withholding Tax:Total") taxAtSource
        ]
          <> catMaybes
            [ mkPosting secondPillarAccount <$> maybePensionFund
            , mkPosting "Equity:Google Deduction Net Amount" <$> maybeDeductionNetAmount
            , mkPosting "Equity:MssbCs Withholding" <$> maybeMssbCsWithholding
            , mkPosting "Expenses:Other" <$> maybeGgive
            , mkPosting "Assets:Debts:Google" <$> maybeGcard
            ]
      )
      & L.set tDescription "Google Salary"
   where
    year :: Integer = toGregorian day ^. L._1
    statePrefix = "State:" <> show year <> ":"

    mkPosting :: AccountName -> Decimal -> Posting
    mkPosting accountName amount =
      makePosting
        (Just Cleared)
        accountName
        (Just $ Cash chf amount)
        NoComment

-- | Transforms text extracted from a Google payslip's PDF into a
-- 'LedgerReport'.
payslipTextToLedger :: Text -> Either Text Transaction
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  return $ payslipToLedger defaultPayslipLedgerConfig payslip
