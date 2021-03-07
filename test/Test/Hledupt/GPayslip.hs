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
                    , deductionsPensionFund = Just 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Nothing
                    , deductionsMssbCsWithholding = Nothing
                    , deductionsGgive = Nothing
                    , deductionsGcard = Nothing
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
                    , deductionsPensionFund = Just 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Nothing
                    , deductionsMssbCsWithholding = Just (-1708)
                    , deductionsGgive = Nothing
                    , deductionsGcard = Nothing
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
            )

      it "Parses a valid payslip 2" $ do
        gpayslip <- Text.readFile "test/data/gpayslip2.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = Just 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Nothing
                    , deductionsMssbCsWithholding = Nothing
                    , deductionsGgive = Nothing
                    , deductionsGcard = Just 123
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
            )

      it "Parses a valid payslip 3" $ do
        gpayslip <- Text.readFile "test/data/gpayslip3.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.55
                    , deductionsPensionFund = Nothing
                    , deductionsTaxAtSource = 0.05
                    , deductionsDeductionNetAmount = Just 1234.00
                    , deductionsMssbCsWithholding = Just (-1234.65)
                    , deductionsGgive = Nothing
                    , deductionsGcard = Nothing
                    , deductionsTotal = 10000.95
                    }
                )
                11234.65
            )

      it "Parses a valid payslip 4" $ do
        gpayslip <- Text.readFile "test/data/gpayslip4.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = Just 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Nothing
                    , deductionsMssbCsWithholding = Nothing
                    , deductionsGgive = Just 15.75
                    , deductionsGcard = Nothing
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
                    , deductionsPensionFund = Just 123.30
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Just 123
                    , deductionsMssbCsWithholding = Just (-1708)
                    , deductionsGgive = Just 123
                    , deductionsGcard = Just 100
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
            \  * State:2020:Withholding Tax:Total  1234.75 CHF\n\
            \  * SecondPillar  123.30 CHF\n\
            \  * Equity:Google Deduction Net Amount  123.00 CHF\n\
            \  * Equity:MssbCs Withholding  -1708.00 CHF\n\
            \  * Expenses:Other  123.00 CHF\n\
            \  * Assets:Debts:Google  100.00 CHF"

      it "Skips optional Fields" $ do
        let config = PayslipLedgerConfig "Bank" "SecondPillar"
        let payslip =
              Payslip
                (fromGregorian 2020 1 24)
                12345.60
                ( Deductions
                    { deductionsSwissSocialSecurity = 1234.50
                    , deductionsUnemploymentInsurance = 123.85 + 123.55
                    , deductionsPensionFund = Nothing
                    , deductionsTaxAtSource = 1234.75
                    , deductionsDeductionNetAmount = Nothing
                    , deductionsMssbCsWithholding = Nothing
                    , deductionsGgive = Nothing
                    , deductionsGcard = Nothing
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
            \  * State:2020:Withholding Tax:Total  1234.75 CHF"
