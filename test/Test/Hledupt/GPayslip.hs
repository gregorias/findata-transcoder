{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.GPayslip (
  tests,
) where

import qualified Data.Text.IO as Text
import Data.Time (fromGregorian)
import Hledupt.GPayslip (
  Payslip (..),
  parsePayslip,
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
