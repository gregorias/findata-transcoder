module Test.Transcoder.GPayslip.PdfToText (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (fromGregorian)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.GPayslip (Deductions (..), Payslip (Payslip))
import qualified Transcoder.GPayslip.PdfToText as PdfToText
import Prelude

tests :: Spec
tests = do
  describe "Transcoder.GPayslip.PdfToText" $ do
    describe "payslipP" $ do
      describe "Parses valid payslips from 'pdftotext -layout'" $ do
        it "Test 0" $ do
          let source = "test/data/gpayslip-pdftotext-layout-0.txt"
          payslipTxt <- decodeUtf8 <$> BS.readFile source
          let payslip = parsePretty PdfToText.payslipP (T.pack source) payslipTxt
          payslip
            `shouldBe` Right
              ( Payslip
                  (fromGregorian 2022 5 25)
                  14836.50
                  ( Deductions
                      { deductionsSwissSocialSecurity = 786.35
                      , deductionsUnemploymentInsurance = 135.85 + 12.45
                      , deductionsPensionFund = Just 810.75
                      , deductionsTaxAtSource = 1767.05
                      , deductionsDeductionNetAmount = Nothing
                      , deductionsMssbCsWithholding = Nothing
                      , deductionsGgive = Nothing
                      , deductionsGcard = Nothing
                      , deductionsTotal = 3512.45
                      }
                  )
                  11324.05
              )
        it "Test 1" $ do
          let source = "test/data/gpayslip-pdftotext-layout-1.txt"
          payslipTxt <- decodeUtf8 <$> BS.readFile source
          let payslip = parsePretty PdfToText.payslipP (T.pack source) payslipTxt
          payslip
            `shouldBe` Right
              ( Payslip
                  (fromGregorian 2020 1 31)
                  7910.55
                  ( Deductions
                      { deductionsSwissSocialSecurity = 1889.90
                      , deductionsUnemploymentInsurance = 184.35
                      , deductionsPensionFund = Nothing
                      , deductionsTaxAtSource = 12987.10 - 3946.70
                      , deductionsDeductionNetAmount = Just 5616.00
                      , deductionsMssbCsWithholding = Just (-11083.65)
                      , deductionsGgive = Nothing
                      , deductionsGcard = Nothing
                      , deductionsTotal = 5647.00
                      }
                  )
                  2263.55
              )
