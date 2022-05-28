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
import Data.Time.Calendar (toGregorian)
import Hledger (Status (Cleared, Pending), Transaction, missingamt, post, transaction)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (pMaybeAmount, pStatus, tDescription)
import Relude
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.GPayslip.Data (Deductions (..), Payslip (..))
import Transcoder.GPayslip.Pdf2Txt (
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
      ( [ post bankAccount missingamt
            & L.set pStatus Pending
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf mainTotal)
        , post "Income:Google" missingamt
            & L.set pStatus Cleared
              . L.set pMaybeAmount (Just $ makeCurrencyAmount chf (-salaryTotal))
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
