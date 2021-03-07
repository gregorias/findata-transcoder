{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.GPayslip (
  tests,
) where

import qualified Data.Text.IO as Text
import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.GPayslip (
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
      it "Parses a valid payslip" $ do
        gpayslip <- Text.readFile "test/data/gpayslip.txt"
        let parsedPayslip = parsePayslip gpayslip
        parsedPayslip
          `shouldBe` Right
            ( Payslip
                (fromGregorian 2020 1 24)
                11234.65
            )
    describe "payslipToTransaction" $ do
      it "Transforms a payslip" $ do
        let config = PayslipLedgerConfig "Bank" "SecondFillar"
        let payslip = Payslip (fromGregorian 2020 1 24) 11234.65
        payslipToTransaction config payslip
          `shouldBe` parseTransactionUnsafe
            "2020/01/24 Google Salary\n\
            \  ! Bank  11234.65 CHF\n\
            \  * Income:Google  -11234.65 CHF\n"
