{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Hledupt.Ib.Csv.RawParse (tests) where

import Hledupt.Ib.Csv.RawParse
import Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.RawParse" $ do
    it "Finds dividend and withholding tax sections." $ do
      let csv =
            "Dividends,Header,Currency,Date,Description,Amount\n\
            \Withholding Tax,Header,Currency,Date,Description,Amount,Code"
          Right stmt = parse csv
      (cDividends stmt, cWithholdingTax stmt)
        `shouldBe` ( "Header,Currency,Date,Description,Amount\n",
                     "Header,Currency,Date,Description,Amount,Code"
                   )
