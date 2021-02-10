{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.CharlesSchwab.Csv (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.CharlesSchwab.Csv (CsCsvRecord (CsCsvRecord), DollarAmount (..), parseCsStatement)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.CharlesSchwab.Csv" $ do
    describe "parseCsStatement" $ do
      it "parses a Charles Schwab Statement" $ do
        let csv =
              "Transactions  for account johndoe  XXXX-0123 as of 01/31/2021 04:06:58 ET\n\
              \\"Date\",\"Action\",\"Symbol\",\"Description\",\"Quantity\",\"Price\",\"Fees & Comm\",\"Amount\",\n\
              \\"01/28/2021\",\"Credit Interest\",\"\",\"SCHWAB1 INT 12/30-01/27\",\"\",\"\",\"\",\"$0.19\",\n\
              \\"01/19/2021\",\"Wire Funds\",\"\",\"WIRED FUNDS DISBURSED\",\"\",\"\",\"\",\"-$123.45\",\n\
              \\"12/31/2020 as of 12/29/2020\",\"Sell\",\"GOOG\",\"ALPHABET INC. CLASS C\",\"5\",\"$1765.2706\",\"$0.20\",\"$8826.15\",\n\
              \\"12/31/2020\",\"Stock Plan Activity\",\"GOOG\",\"ALPHABET INC. CLASS C\",\"5\",\"\",\"\",\"\",\n\
              \Transactions Total,\"\",\"\",\"\",\"\",\"\",\"\",$0.19"
        parseCsStatement csv
          `shouldBe` Right
            [ CsCsvRecord
                (fromGregorian 2021 1 28)
                "Credit Interest"
                ""
                "SCHWAB1 INT 12/30-01/27"
                Nothing
                Nothing
                Nothing
                (Just $ DollarAmount (fromRational $ 19 % 100))
            , CsCsvRecord
                (fromGregorian 2021 1 19)
                "Wire Funds"
                ""
                "WIRED FUNDS DISBURSED"
                Nothing
                Nothing
                Nothing
                (Just $ DollarAmount (fromRational $ -12345 % 100))
            , CsCsvRecord
                (fromGregorian 2020 12 31)
                "Sell"
                "GOOG"
                "ALPHABET INC. CLASS C"
                (Just 5)
                (Just $ DollarAmount (fromRational $ 17652706 % 10000))
                (Just $ DollarAmount (fromRational $ 20 % 100))
                (Just $ DollarAmount (fromRational $ 882615 % 100))
            , CsCsvRecord
                (fromGregorian 2020 12 31)
                "Stock Plan Activity"
                "GOOG"
                "ALPHABET INC. CLASS C"
                (Just 5)
                Nothing
                Nothing
                Nothing
            ]
