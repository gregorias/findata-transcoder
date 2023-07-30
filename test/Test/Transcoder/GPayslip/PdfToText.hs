{-# LANGUAGE ImportQualifiedPost #-}

module Test.Transcoder.GPayslip.PdfToText (tests) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (fromGregorian)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.GPayslip.PdfToText (
  Earnings (..),
  Item (..),
  NotionalPay (..),
  OtherPaymentsAndDeductions (..),
  PaymentDetails (..),
  Payslip (..),
  PayslipSummary (..),
  StatutoryDeductions (..),
 )
import Transcoder.GPayslip.PdfToText qualified as PdfToText
import Prelude

-- | Read a UTF-8 encoded file.
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 = fmap decodeUtf8 . BS.readFile

tests :: Spec
tests = do
  describe "Transcoder.GPayslip.PdfToText" $ do
    describe "payslipP" $ do
      describe "Parses valid payslips from 'pdftotext -layout'" $ do
        it "Test January 2023" $ do
          let source = "./test/data/gpayslip-202301.txt"
          payslipTxt <- readFileUtf8 source
          let payslip = parsePretty PdfToText.payslipP (T.pack source) payslipTxt
          payslip
            `shouldBe` Right
              ( Payslip
                  (fromGregorian 2023 1 25)
                  ( PayslipSummary
                      { payslipSummaryEarnings = 37932.85
                      , payslipSummaryStatutoryDeductions = 9014.05
                      , payslipSummaryOtherPaymentsAndDeductions = -1010.75
                      , payslipSummaryNetPay = 27908.05
                      }
                  )
                  ( Earnings
                      { earningsMonthlySalary = Item 0 14461.55
                      , earningsErHealthInsurance = Item 0 375.00
                      , earningsAnnualBonusGross = Just (Item 0 23000.00)
                      , earningsBonusGross = Nothing
                      , earningsSpotBonusGross = Nothing
                      , earningsNonCashSpotBonusGrossUp = Nothing
                      , earningsMealAllowanceGrossUp = Just (Item 0 96.30)
                      , earningsPeerBonus = Nothing
                      , earningsEducationSubsidyGross = Nothing
                      , earningsTotal = Item 0 37932.85
                      }
                  )
                  ( NotionalPay
                      { notionalPayTotal = Item 0 236.50
                      , notionalPayNonCashSpotBonus = Nothing
                      , notionalPayMealAllowanceNet = Just (Item 0 236.50)
                      , notionalPayGsuGainIncomeTaxes = Nothing
                      , notionalPayGsuGainSocialSecurity = Nothing
                      }
                  )
                  ( StatutoryDeductions
                      { statutoryDeductionsWht = Item 0 6855.20
                      , statutoryDeductionsUnemploymentInsurance = Item 0 135.85
                      , statutoryDeductionsSwissSocialSecurity = Item 0 2023.00
                      , statutoryDeductionsTotal = Item 0 9014.05
                      }
                  )
                  ( OtherPaymentsAndDeductions
                      { otherPaymentsAndDeductionsTotal = Item 0 (-1010.75)
                      , otherPaymentsAndDeductionsPensionContributionEe = Item 0 (-810.75)
                      , otherPaymentsAndDeductionsMssbWitholdingCredit = Nothing
                      , otherPaymentsAndDeductionsGcardRepayment = Nothing
                      , otherPaymentsAndDeductionsGgiveDeductions = Just (Item 0 (-200.00))
                      }
                  )
                  27908.05
                  ( PaymentDetails
                      { paymentDetailsBankName = "Banque Cantonale de Geneve"
                      , paymentDetailsBankAccountIban = "XXXXXXXX0000"
                      , paymentDetailsAmount = 27908.05
                      }
                  )
              )
        it "Test February 2023" $ do
          let source = "./test/data/gpayslip-202302.txt"
          payslipTxt <- readFileUtf8 source
          let payslip = parsePretty PdfToText.payslipP (T.pack source) payslipTxt
          payslip
            `shouldBe` Right
              ( Payslip
                  (fromGregorian 2023 2 24)
                  ( PayslipSummary
                      { payslipSummaryEarnings = 15008.45
                      , payslipSummaryStatutoryDeductions = 2662.40
                      , payslipSummaryOtherPaymentsAndDeductions = -968.15
                      , payslipSummaryNetPay = 11377.90
                      }
                  )
                  ( Earnings
                      { earningsMonthlySalary = Item 0 14461.55
                      , earningsErHealthInsurance = Item 0 375.00
                      , earningsEducationSubsidyGross = Just (Item 0 49.00)
                      , earningsNonCashSpotBonusGrossUp = Nothing
                      , earningsMealAllowanceGrossUp = Just (Item 122.90 0)
                      , earningsSpotBonusGross = Nothing
                      , earningsPeerBonus = Nothing
                      , earningsBonusGross = Nothing
                      , earningsAnnualBonusGross = Nothing
                      , earningsTotal = Item 122.90 14885.55
                      }
                  )
                  ( NotionalPay
                      { notionalPayMealAllowanceNet = Just (Item 322.50 0)
                      , notionalPayNonCashSpotBonus = Nothing
                      , notionalPayGsuGainIncomeTaxes = Nothing
                      , notionalPayGsuGainSocialSecurity = Nothing
                      , notionalPayTotal = Item 322.50 0
                      }
                  )
                  ( StatutoryDeductions
                      { statutoryDeductionsWht = Item 0 1714.00
                      , statutoryDeductionsSwissSocialSecurity = Item 23.60 788.95
                      , statutoryDeductionsUnemploymentInsurance = Item 0 135.85
                      , statutoryDeductionsTotal = Item 23.60 2638.80
                      }
                  )
                  ( OtherPaymentsAndDeductions
                      { otherPaymentsAndDeductionsPensionContributionEe = Item 0 (-810.75)
                      , otherPaymentsAndDeductionsMssbWitholdingCredit = Nothing
                      , otherPaymentsAndDeductionsGcardRepayment = Just (Item 0 (-157.40))
                      , otherPaymentsAndDeductionsGgiveDeductions = Nothing
                      , otherPaymentsAndDeductionsTotal = Item 0 (-968.15)
                      }
                  )
                  11377.90
                  ( PaymentDetails
                      { paymentDetailsBankName = "Banque Cantonale de Geneve"
                      , paymentDetailsBankAccountIban = "XXXXXXXX0000"
                      , paymentDetailsAmount = 11377.90
                      }
                  )
              )
        it "Test March 2023" $ do
          let source = "./test/data/gpayslip-202303.txt"
          payslipTxt <- readFileUtf8 source
          let payslip = parsePretty PdfToText.payslipP (T.pack source) payslipTxt
          payslip
            `shouldBe` Right
              ( Payslip
                  (fromGregorian 2023 3 24)
                  ( PayslipSummary
                      { payslipSummaryEarnings = 28374.60
                      , payslipSummaryStatutoryDeductions = 6276.45
                      , payslipSummaryOtherPaymentsAndDeductions = -858.19
                      , payslipSummaryNetPay = 21239.96
                      }
                  )
                  ( Earnings
                      { earningsMonthlySalary = Item 0 15307.70
                      , earningsErHealthInsurance = Item 0 375.00
                      , earningsAnnualBonusGross = Just (Item 0 10500.00)
                      , earningsBonusGross = Just (Item 0 1833.00)
                      , earningsSpotBonusGross = Nothing
                      , earningsPeerBonus = Just (Item 200.00 0.00)
                      , earningsEducationSubsidyGross = Nothing
                      , earningsNonCashSpotBonusGrossUp = Nothing
                      , earningsMealAllowanceGrossUp = Just (Item 0 158.90)
                      , earningsTotal = Item 200.00 28174.60
                      }
                  )
                  ( NotionalPay
                      { notionalPayNonCashSpotBonus = Nothing
                      , notionalPayMealAllowanceNet = Just (Item 0 387)
                      , notionalPayGsuGainIncomeTaxes = Nothing
                      , notionalPayGsuGainSocialSecurity = Nothing
                      , notionalPayTotal = Item 0 387
                      }
                  )
                  ( StatutoryDeductions
                      { statutoryDeductionsWht = Item 0 4616.25
                      , statutoryDeductionsSwissSocialSecurity = Item 10.60 1513.75
                      , statutoryDeductionsUnemploymentInsurance = Item 0 135.85
                      , statutoryDeductionsTotal = Item 10.60 6265.85
                      }
                  )
                  ( OtherPaymentsAndDeductions
                      { otherPaymentsAndDeductionsPensionContributionEe = Item 0 (-858.19)
                      , otherPaymentsAndDeductionsMssbWitholdingCredit = Nothing
                      , otherPaymentsAndDeductionsGcardRepayment = Nothing
                      , otherPaymentsAndDeductionsGgiveDeductions = Nothing
                      , otherPaymentsAndDeductionsTotal = Item 0 (-858.19)
                      }
                  )
                  21239.96
                  ( PaymentDetails
                      { paymentDetailsBankAccountIban = "XXXXXXXX0000"
                      , paymentDetailsBankName = "Banque Cantonale de Geneve"
                      , paymentDetailsAmount = 21239.96
                      }
                  )
              )
