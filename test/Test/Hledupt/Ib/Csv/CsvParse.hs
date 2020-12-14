{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.CsvParse (tests) where

import qualified Data.Csv as Csv
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.Ib.Csv.CsvParse
import Test.Hspec
import Text.Megaparsec.Match ((=~))

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.CsvParse" $ do
    describe "parseMtmStatement" $ do
      it "Parses a range date correctly" $ do
        let csvs =
              Map.fromList
                [ ( "Statement",
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n"
                  )
                ]
        (sLastStatementDay <$> parseMtmStatement csvs)
          `shouldBe` Right (fromGregorian 2020 12 4)

      it "Parses Csvs" $ do
        let csv =
              Map.fromList
                [ ( "Statement",
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n"
                  ),
                  ( "Positions",
                    "Header,Asset Class,Currency,Symbol,Description,Prior Quantity,Quantity,Prior Price,Price,Prior Market Value,Market Value,Position,Trading,Comm.,Other,Total\n"
                  ),
                  ( "Dividends",
                    "Header,Currency,Date,Description,Amount\n\
                    \Data,USD,2019-12-20,ACWF(US46434V3160) Cash Dividend USD 0.42537 per Share (Ordinary Dividend),1050.24\n\
                    \Data,Total,,,10813.42"
                  ),
                  ( "Withholding Tax",
                    "Header,Currency,Date,Description,Amount,Code\n\
                    \Data,USD,2019-09-06,BND(US9219378356) Cash Dividend USD 0.188198 per Share - US Tax,-2.72\n\
                    \Data,Total,,,-1585.38,"
                  )
                ]
        parseMtmStatement csv
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
                    ],
                  sWithholdingTaxes =
                    [ WithholdingTaxRecord $
                        WithholdingTax
                          (fromGregorian 2019 9 6)
                          "BND"
                          (fromRational $ -272 % 100),
                      TotalWithholdingTaxRecord
                    ]
                }
            )
      it "Gives a readable error string when taxes can't be parsed." $ do
        let csv =
              [ ( "Statement",
                  "Header,Field Name,Field Value\n\
                  \Data,Period,\"December 11, 2019 - December 4, 2020\"\n"
                ),
                ("Withholding Tax", "Mangled CSV")
              ]
        parseMtmStatement csv
          `shouldSatisfy` \case
            Left errorMsg -> (errorMsg =~ "Could not parse taxes data.")
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

    describe "WithholdingTaxRecord" $ do
      it "FromNamedRecord parses a Quellensteuer entry" $ do
        let csv =
              "Header,Currency,Date,Description,Amount,Code\n\
              \Data,USD,2019-09-06,BND(US9219378356) Cash Dividend USD 0.188198 per Share - US Tax,-2.72"
        fmap snd (Csv.decodeByName csv)
          `shouldBe` Right
            [ WithholdingTaxRecord $
                WithholdingTax
                  (fromGregorian 2019 9 6)
                  "BND"
                  (fromRational $ -272 % 100)
            ]
      it "FromNamedRecord parses a total entry" $ do
        let csv =
              "Header,Currency,Date,Description,Amount,Code\n\
              \Data,Total,,,-1585.38,\n"
        fmap snd (Csv.decodeByName csv)
          `shouldBe` Right [TotalWithholdingTaxRecord]
