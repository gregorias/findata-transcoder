{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.CsvParse (tests) where

import qualified Data.Csv as Csv
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.Ib.Csv.CsvParse
import Hledupt.Ib.Csv.RawParse (Csvs (..), nullcsvs)
import Test.Hspec

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.CsvParse" $ do
    describe "parse" $ do
      it "parses a range date correctly" $ do
        let csv =
              nullcsvs
                { cStatement =
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n",
                  cPositions =
                    "Positions and Mark-to-Market Profit and Loss,Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
                }
        (sLastStatementDay <$> parse csv)
          `shouldBe` Right (fromGregorian 2020 12 4)

    describe "CashMovement" $ do
      it "Parses CashMovement Lines" $ do
        let csv =
              "Header,Currency,Settle Date,Description,Amount\n\
              \Data,CHF,2020-01-20,title,100.32"
        fmap snd (Csv.decodeByName csv)
          `shouldBe` Right
            [ CashMovement
                (fromGregorian 2020 1 20)
                CHF
                (fromRational (10032 % 100))
            ]
