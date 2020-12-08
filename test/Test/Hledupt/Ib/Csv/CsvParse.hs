{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.CsvParse (tests) where

import qualified Data.Csv as Csv
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.Ib.Csv.CsvParse
import Hledupt.Ib.Csv.RawParse (Csvs (..), nullcsvs)
import Test.Hspec
import Text.Megaparsec.Match ((=~))

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
                    "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
                }
        (sLastStatementDay <$> parse csv)
          `shouldBe` Right (fromGregorian 2020 12 4)

      it "Parses Csvs" $ do
        let csv =
              nullcsvs
                { cStatement =
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n",
                  cPositions =
                    "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n",
                  cDividends =
                    "Header,Currency,Date,Description,Amount\n\
                    \Data,USD,2019-12-20,ACWF(US46434V3160) Cash Dividend USD 0.42537 per Share (Ordinary Dividend),1050.24\n\
                    \Data,Total,,,10813.42"
                }
        parse csv
          `shouldBe` Right
            ( Statement
                { sLastStatementDay = fromGregorian 2020 12 4,
                  sPositionRecords = [],
                  sCashMovements = [],
                  sDividends =
                    [ DividendRecord $
                        Dividend
                          (fromGregorian 2019 12 20)
                          "ACWF"
                          (fromRational $ 42537 % 100000)
                          (fromRational $ 105024 % 100),
                      TotalDividendsRecord
                    ]
                }
            )
      -- Add a readable error message for every failure
      -- Use https://hackage.haskell.org/package/replace-megaparsec-1.4.4.0/docs/Replace-Megaparsec.html for pattern matching
      it "Gives a readable error string when dividends can't be parsed." $ do
        let csv =
              nullcsvs
                { cStatement =
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n",
                  cPositions =
                    "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n",
                  cDividends = "Mangled CSV"
                }
        parse csv
          `shouldSatisfy` \case
            Left errorMsg -> (errorMsg =~ "Could not parse dividends data.")
            Right _ -> False

    describe "CashMovement" $ do
      it "FromNamedRecord parses CashMovement lines" $ do
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

    describe "Dividend" $ do
      it "FromNamedRecord parses Dividend lines" $ do
        let csv =
              "Header,Currency,Date,Description,Amount\n\
              \Data,USD,2019-12-20,ACWF(US46434V3160) Cash Dividend USD 0.42537 per Share (Ordinary Dividend),1050.24"
        fmap snd (Csv.decodeByName csv)
          `shouldBe` Right
            [ Dividend
                (fromGregorian 2019 12 20)
                "ACWF"
                (fromRational $ 42537 % 100000)
                (fromRational $ 105024 % 100)
            ]
