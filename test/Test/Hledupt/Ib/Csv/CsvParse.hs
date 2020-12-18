{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.CsvParse (tests) where

import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.Ib.Csv.CsvParse
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Text.Megaparsec.Match ((=~))

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.CsvParse" $ do
    let stmtWithDateOnly =
          Map.fromList
            [ ( "Statement",
                "Header,Field Name,Field Value\n\
                \Data,Period,\"December 11, 2019 - December 4, 2020\"\n"
              )
            ]
    describe "parseActivityStatement" $ do
      it "Parses Csvs" $ do
        let csv =
              Map.fromList
                [ ( "Statement",
                    "Header,Field Name,Field Value\n\
                    \Data,Period,\"December 11, 2019 - December 4, 2020\"\n"
                  ),
                  ( "Cash Report",
                    "Header,Currency Summary,Currency,Total,Securities,Futures,Month to Date,Year to Date,\n\
                    \Data,Starting Cash,Base Currency Summary,1.0,1.0,0,,,\n\
                    \Data,Commissions,Base Currency Summary,-1.0,-1.0,0,0,-1.0,\n\
                    \Data,Deposits,Base Currency Summary,0,0,0,0,1.0,\n\
                    \Data,Withdrawals,Base Currency Summary,0,0,0,0,-1.0,\n\
                    \Data,Dividends,Base Currency Summary,1,1,0,0,1.0,\n\
                    \Data,Broker Interest Paid and Received,Base Currency Summary,0,0,0,0,1.36283,\n\
                    \Data,Net Trades (Sales),Base Currency Summary,0,0,0,0,1.0,\n\
                    \Data,Net Trades (Purchase),Base Currency Summary,-1.0,-1.30478,0,0,-4.0,\n\
                    \Data,Withholding Tax,Base Currency Summary,-3.,-3.,0,0,-8.0,\n\
                    \Data,Cash FX Translation Gain/Loss,Base Currency Summary,-1.0,-1.63908444,0,,,\n\
                    \Data,Ending Cash,Base Currency Summary,1.204361315,1.204361315,0,,,\n\
                    \Data,Ending Settled Cash,Base Currency Summary,1.204361315,1.204361315,0,,,\n\
                    \Data,Starting Cash,CHF,10.0011305,10.011305,0,,,\n\
                    \Data,Commissions,CHF,0,0,0,0,-1.93502,\n\
                    \Data,Withdrawals,CHF,0,0,0,0,-1.32,\n\
                    \Data,Net Trades (Sales),CHF,0,0,0,0,1.76,\n\
                    \Data,Ending Cash,CHF,100.0011,100.0011,0,,,\n\
                    \Data,Ending Settled Cash,CHF,100.0011305,100.0011305,0,,,\n\
                    \Data,Starting Cash,USD,1.022015641,1.022015641,0,,,\n\
                    \Data,Commissions,USD,-1.71996,-1.71996,0,0,-6.08938,\n\
                    \Data,Deposits,USD,0,0,0,0,1.56,\n\
                    \Data,Dividends,USD,1.6,1.6,0,0,1.22,\n\
                    \Data,Broker Interest Paid and Received,USD,0,0,0,0,1.4,\n\
                    \Data,Net Trades (Purchase),USD,-1.96,-1.96,0,0,-4.02,\n\
                    \Data,Withholding Tax,USD,-3.88,-3.88,0,0,-9.85,\n\
                    \Data,Ending Cash,USD,6.06,6.06,0,,,\n\
                    \Data,Ending Settled Cash,USD,6.06,6.06,0,,,"
                  ),
                  ( "Open Positions",
                    "Header,DataDiscriminator,Asset Category,Currency,Symbol,Quantity,Mult,Cost Price,Cost Basis,Close Price,Value,Unrealized P/L,Unrealized P/L %,Code\n\
                    \Data,Summary,Stocks,USD,VOO,7,1,1.0,1.0,336.55,2000.1,1.0,0.09,\n\
                    \Total,,Stocks,USD,,,,,1.39573,,1.9,1.50427,,\n"
                  ),
                  ( "Trades",
                    "Header,DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,C. Price,Proceeds,Comm/Fee,Basis,Realized P/L,Realized P/L %,MTM P/L,Code\n\
                    \Data,Order,Stocks,USD,VOO,\"2020-10-05, 09:52:53\",2,309.35,312.08,-618.7,-0.6187,619.3187,0,0,5.46,O;R\n\
                    \SubTotal,,Stocks,USD,VOO,,2,,,-618.7,-0.6187,619.3187,0,0.00,1.00,\n\
                    \Total,,Stocks,USD,,,,,,-1719.96,-1.71996,1721.67996,0,,1.00, \n"
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
        parseActivityStatement csv
          `shouldBe` Right
            ( ActivityStatement
                { asLastStatementDay = fromGregorian 2020 12 4,
                  asCashPositions =
                    [ EndingCash "CHF" (fromRational $ 1000011 % 10000),
                      EndingCash "USD" (fromRational $ 606 % 100)
                    ],
                  asStockPositions =
                    [ StockPosition "VOO" 7 (fromRational $ 33655 % 100)
                    ],
                  asCashMovements =
                    [],
                  asTrades =
                    [ Trade (fromGregorian 2020 10 5) "VOO" 2 (fromRational $ -6187 % 10) (fromRational $ -6187 % 10000)
                    ],
                  asDividends =
                    [ Dividend
                        (fromGregorian 2019 12 20)
                        "ACWF"
                        (fromRational $ 42537 % 100000)
                        (fromRational $ 105024 % 100)
                    ],
                  asTaxes =
                    [ WithholdingTax
                        (fromGregorian 2019 9 6)
                        "BND"
                        (fromRational $ -272 % 100)
                    ]
                }
            )

    describe "parseMtmStatement" $ do
      it "Parses a range date correctly" $ do
        (mtmsLastStatementDay <$> parseMtmStatement stmtWithDateOnly)
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
                  ( "Deposits & Withdrawals",
                    "Header,Currency,Settle Date,Description,Amount\n\
                    \Data,CHF,2020-01-20,title,100.32"
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
            ( MtmStatement
                { mtmsLastStatementDay = fromGregorian 2020 12 4,
                  mtmsPositions = [],
                  mtmsCashMovements =
                    [ CashMovement
                        (fromGregorian 2020 1 20)
                        CHF
                        (fromRational (10032 % 100))
                    ],
                  mtmsDividends =
                    [ Dividend
                        (fromGregorian 2019 12 20)
                        "ACWF"
                        (fromRational $ 42537 % 100000)
                        (fromRational $ 105024 % 100)
                    ],
                  mtmsWithholdingTaxes =
                    [ WithholdingTax
                        (fromGregorian 2019 9 6)
                        "BND"
                        (fromRational $ -272 % 100)
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
            Left errorMsg -> (errorMsg =~ "Could not parse Withholding Tax records.")
            Right _ -> False
