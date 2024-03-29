{-# LANGUAGE ImportQualifiedPost #-}

module Test.Transcoder.GPayslip (
  tests,
) where

import Data.Text.IO qualified as T
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.HUnit.Extra (assertRightOrFailPrint)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.GPayslip (
  payslipTextToLedger,
 )

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.GPayslip" $ do
    describe "payslipTextToLedger" $ do
      it "Test January 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202301.txt"
        payslipTextToLedger payslip
          `shouldBe` Right
            [transactionQQ|
             2023/01/25 Google Salary
               ! Assets:Liquid:BCGE  27908.05 CHF
               * Income:Google  -37932.85 CHF
               * State:2023:Mandatory Contributions:Social Security  2023.00 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  6855.20 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  810.75 CHF
               * Expenses:gGive  200 CHF|]
      it "Test February 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202302.txt"
        payslipTextToLedger payslip
          `shouldBe` Right
            [transactionQQ|
             2023/02/24 Google Salary
               ! Assets:Liquid:BCGE  11377.90 CHF
               * Income:Google  -15008.45 CHF
               * State:2023:Mandatory Contributions:Social Security  812.55 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  1714.00 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  810.75 CHF
               * Assets:Debts:Google  157.40 CHF|]
      it "Test March 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202303.txt"
        payslipTextToLedger payslip
          `shouldBe` Right
            [transactionQQ|
             2023/03/24 Google Salary
               ! Assets:Liquid:BCGE  21239.96 CHF
               * Income:Google  -28374.60 CHF
               * State:2023:Mandatory Contributions:Social Security  1524.35 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  4616.25 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  858.19 CHF|]
      it "Test April 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202304.txt"
        payslipTextToLedger payslip
          `shouldBe` Right
            [transactionQQ|
             2023/04/25 Google Salary
               ! Assets:Liquid:BCGE  13587.11 CHF
               * Income:Google      -16082.70 CHF
               * State:2023:Mandatory Contributions:Social Security  1374.80 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  3942.70 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  858.19 CHF
               * Assets:Debts:Google  6.25 CHF
               * Equity:MssbCs Withholding  -3822.20 CHF|]
      it "Test May 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202305.txt"
        payslipTextToLedger payslip
          `shouldBe` Right
            [transactionQQ|
             2023/05/25 Google Salary
               ! Assets:Liquid:BCGE  12286.91 CHF
               * Income:Google      -15837.50 CHF
               * State:2023:Mandatory Contributions:Social Security  975.00 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  2297.75 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  858.19 CHF
               * Assets:Debts:Google  95.80 CHF
               * Equity:MssbCs Withholding  -812.00 CHF
              |]
      it "Test July 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202307.txt"
        ledger <- assertRightOrFailPrint $ payslipTextToLedger payslip
        ledger
          `shouldBe` [transactionQQ|
             2023/07/25 Google Salary
               ! Assets:Liquid:BCGE  14868.71 CHF
               * Income:Google      -16332.15 CHF
               * State:2023:Mandatory Contributions:Social Security        2067.35 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  7036.85 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  858.19 CHF
               * Equity:MssbCs Withholding  -8634.80 CHF
              |]
      it "Test July 2023" $ do
        payslip <- T.readFile "./test/data/gpayslip-202312.txt"
        ledger <- assertRightOrFailPrint $ payslipTextToLedger payslip
        ledger
          `shouldBe` [transactionQQ|
             2023/12/21 Google Salary
               ! Assets:Liquid:BCGE  23343.36 CHF
               * Income:Google      -31048.10 CHF
               * State:2023:Mandatory Contributions:Social Security        1812.85 CHF
               * State:2023:Mandatory Contributions:Unemployment Insurance  135.85 CHF
               * State:2023:Withholding Tax:Total  5914.15 CHF
               * Assets:Illiquid:AXA Wintherthur Pension Fund  858.19 CHF
               * Equity:MssbCs Withholding   -1016.30 CHF
              |]
