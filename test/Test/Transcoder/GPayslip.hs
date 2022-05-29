module Test.Transcoder.GPayslip (
  tests,
) where

import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.GPayslip (
  Deductions (..),
  Payslip (..),
  PayslipLedgerConfig (PayslipLedgerConfig),
  payslipToLedger,
 )

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.GPayslip" $ do
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
        payslipToLedger config payslip
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
        payslipToLedger config payslip
          `shouldBe` parseTransactionUnsafe
            "2020/01/24 Google Salary\n\
            \  ! Bank  11234.65 CHF\n\
            \  * Income:Google  -12345.60 CHF\n\
            \  * State:2020:Mandatory Contributions:Social Security  1234.50 CHF\n\
            \  * State:2020:Mandatory Contributions:Unemployment Insurance  247.40 CHF\n\
            \  * State:2020:Withholding Tax:Total  1234.75 CHF"
