{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.GPayslip (
  tests,
) where

import qualified Data.Text.IO as Text
import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.GPayslip (
  Deductions (..),
  Payslip (..),
  PayslipLedgerConfig (PayslipLedgerConfig),
  parsePayslip,
  payslipToTransaction,
 )
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.GPayslip" $ do
    describe "parsePayslip" $ do
      it "Parses a valid payslip 0" $ do
        gpayslip <- Text.readFile "test/data/gpayslip0.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsMssbCsWithholding = -0
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
            )
      it "Parses a valid payslip 1" $ do
        gpayslip <- Text.readFile "test/data/gpayslip1.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsMssbCsWithholding = -1708
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
            )
    describe "payslipToTransaction" $ do
      it "Transforms a payslip" $ do
        let config = PayslipLedgerConfig "Bank" "SecondPillar"
        let payslip =
              Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsMssbCsWithholding = -1708
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
        payslipToTransaction config payslip
          `shouldBe` parseTransactionUnsafe
            "2020/01/24 Google Salary\n\
            \  ! Bank  11234.65 CHF\n\
            \  * Income:Google  -12345.60 CHF\n\
            \  * State:2020:Mandatory Contributions:Social Security  1234.50 CHF\n\
            \  * State:2020:Mandatory Contributions:Unemployment Insurance  247.40 CHF\n\
            \  * SecondPillar  123.30 CHF\n\
            \  * Equity:MssbCsWithholding  -1708.00 CHF\n\
            \  * State:2020:Withholding Tax:Total  1234.75 CHF"
