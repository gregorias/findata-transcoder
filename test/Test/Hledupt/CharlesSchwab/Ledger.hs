{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.CharlesSchwab.Ledger (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord),
  DollarAmount (..),
 )
import Hledupt.CharlesSchwab.Ledger (csvToLedger)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.CharlesSchwab.Ledger" $ do
    describe "csvToLedger" $ do
      it "transforms CSV entries" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2021 1 19)
                  "Wire Funds"
                  ""
                  "WIRED FUNDS DISBURSED"
                  Nothing
                  Nothing
                  Nothing
                  (DollarAmount (fromRational $ -12345 % 100))
              ]
        csvToLedger entries
          `shouldBe` Left "Unimplemented"
