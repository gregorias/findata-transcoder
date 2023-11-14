{-# LANGUAGE ImportQualifiedPost #-}

-- | This module parses a text dump from a Google Payslip and outputs a ledger.
module Transcoder.GPayslip (
  payslipTextToLedger,
  PayslipLedgerConfig (..),
) where

import Control.Lens ((^.))
import Control.Lens qualified as L
import Data.Cash (Cash (Cash))
import Data.Decimal (Decimal)
import Data.Time.Calendar (toGregorian)
import Hledger (AccountName, Posting, Status (Cleared, Pending), Transaction, transaction)
import Hledger.Data.Extra (Comment (NoComment), ToAmount (toAmount), makePosting)
import Hledger.Data.Lens (tDescription)
import Relude
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.GPayslip.PdfToText (
  Item (..),
  OtherPaymentsAndDeductions (..),
  PaymentDetails (..),
  Payslip (..),
  PayslipSummary (payslipSummaryEarnings),
  StatutoryDeductions (StatutoryDeductions),
  payslipP,
 )

data PayslipLedgerConfig = PayslipLedgerConfig
  { payslipLedgerConfigBankAccount :: !Text
  -- ^ The bank account Google sends the salary to.
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
parsePayslip = parsePretty payslipP "Google payslip"

payslipToLedger :: PayslipLedgerConfig -> Payslip -> Transaction
payslipToLedger
  (PayslipLedgerConfig bankAccount secondPillarAccount)
  ( Payslip
      day
      summary
      _earnings
      _notionalPay
      (StatutoryDeductions wht socialSecurity unemploymentInsurance _)
      ( OtherPaymentsAndDeductions
          { otherPaymentsAndDeductionsTotal = _
          , otherPaymentsAndDeductionsPensionContributionEe = pensionContributionEe
          , otherPaymentsAndDeductionsMssbWitholdingCredit = mssbWitholdingCredit
          , otherPaymentsAndDeductionsGgiveDeductions = gGive
          , otherPaymentsAndDeductionsGcardRepayment = gCardRepayment
          }
        )
      _netPay
      (PaymentDetails _ _ paymentAmount)
    ) =
    transaction
      day
      ( [ makePosting (Just Pending) bankAccount (Just . toAmount $ Cash chf paymentAmount) NoComment
        , mkPosting "Income:Google" (-payslipSummaryEarnings summary)
        , mkPosting (statePrefix <> "Mandatory Contributions:Social Security") (itemToTotal socialSecurity)
        , mkPosting (statePrefix <> "Mandatory Contributions:Unemployment Insurance") (itemToTotal unemploymentInsurance)
        , mkPosting (statePrefix <> "Withholding Tax:Total") (itemToTotal wht)
        , mkPosting secondPillarAccount (-(itemToTotal pensionContributionEe))
        ]
          <> catMaybes
            [ mkPosting "Expenses:gGive" . negate . itemToTotal <$> gGive
            , mkPosting "Assets:Debts:Google" . negate . itemToTotal <$> gCardRepayment
            , mkPosting "Equity:MssbCs Withholding" . negate . itemToTotal <$> mssbWitholdingCredit
            ]
      )
      & L.set tDescription "Google Salary"
   where
    year :: Integer = toGregorian day ^. L._1
    statePrefix = "State:" <> show year <> ":"

    itemToTotal :: Item -> Decimal
    itemToTotal (Item prior current) = prior + current

    mkPosting :: AccountName -> Decimal -> Posting
    mkPosting accountName amount =
      makePosting
        (Just Cleared)
        accountName
        (Just . toAmount $ Cash chf amount)
        NoComment

-- | Transforms text extracted from a Google payslip's PDF into a
-- 'LedgerReport'.
payslipTextToLedger :: Text -> Either Text Transaction
payslipTextToLedger payslipText = do
  payslip <- parsePayslip payslipText
  return $ payslipToLedger defaultPayslipLedgerConfig payslip
