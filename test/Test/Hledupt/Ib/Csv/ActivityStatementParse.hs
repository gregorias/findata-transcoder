{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Ib.Csv.ActivityStatementParse (tests) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledupt.Ib.Csv.ActivityStatementParse
import Hledupt.Ib.Csv.RawParse
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import Text.Megaparsec.Match ((=~))

tests :: SpecWith ()
tests = do
  describe "Hledupt.Ib.Csv.ActivityStatementParse" $ do
    describe "parseActivityStatement" $ do
      it "Parses a range date correctly" $ do
        let stmtWithDateOnly =
              Statement
                [ ( "Statement",
                    Section
                      [ "Field Name,Field Value\n\
                        \Period,\"December 11, 2019 - December 4, 2020\"\n"
                      ]
                  )
                ]
        (asLastStatementDay <$> parseActivityStatement stmtWithDateOnly)
          `shouldBe` Right (fromGregorian 2020 12 4)

      it "Parses Csvs" $ do
        let csv =
              Statement
                [ ( "Statement",
                    Section
                      [ "Field Name,Field Value\n\
                        \Period,\"December 11, 2019 - December 4, 2020\"\n"
                      ]
                  ),
                  ( "Cash Report",
                    Section
                      [ "Currency Summary,Currency,Total,Securities,Futures,Month to Date,Year to Date,\n\
                        \Starting Cash,Base Currency Summary,1.0,1.0,0,,,\n\
                        \Commissions,Base Currency Summary,-1.0,-1.0,0,0,-1.0,\n\
                        \Deposits,Base Currency Summary,0,0,0,0,1.0,\n\
                        \Withdrawals,Base Currency Summary,0,0,0,0,-1.0,\n\
                        \Dividends,Base Currency Summary,1,1,0,0,1.0,\n\
                        \Broker Interest Paid and Received,Base Currency Summary,0,0,0,0,1.36283,\n\
                        \Net Trades (Sales),Base Currency Summary,0,0,0,0,1.0,\n\
                        \Net Trades (Purchase),Base Currency Summary,-1.0,-1.30478,0,0,-4.0,\n\
                        \Withholding Tax,Base Currency Summary,-3.,-3.,0,0,-8.0,\n\
                        \Cash FX Translation Gain/Loss,Base Currency Summary,-1.0,-1.63908444,0,,,\n\
                        \Ending Cash,Base Currency Summary,1.204361315,1.204361315,0,,,\n\
                        \Ending Settled Cash,Base Currency Summary,1.204361315,1.204361315,0,,,\n\
                        \Starting Cash,CHF,10.0011305,10.011305,0,,,\n\
                        \Commissions,CHF,0,0,0,0,-1.93502,\n\
                        \Withdrawals,CHF,0,0,0,0,-1.32,\n\
                        \Net Trades (Sales),CHF,0,0,0,0,1.76,\n\
                        \Ending Cash,CHF,100.0011,100.0011,0,,,\n\
                        \Ending Settled Cash,CHF,100.0011305,100.0011305,0,,,\n\
                        \Starting Cash,USD,1.022015641,1.022015641,0,,,\n\
                        \Commissions,USD,-1.71996,-1.71996,0,0,-6.08938,\n\
                        \Deposits,USD,0,0,0,0,1.56,\n\
                        \Dividends,USD,1.6,1.6,0,0,1.22,\n\
                        \Broker Interest Paid and Received,USD,0,0,0,0,1.4,\n\
                        \Net Trades (Purchase),USD,-1.96,-1.96,0,0,-4.02,\n\
                        \Withholding Tax,USD,-3.88,-3.88,0,0,-9.85,\n\
                        \Ending Cash,USD,6.06,6.06,0,,,\n\
                        \Ending Settled Cash,USD,6.06,6.06,0,,,"
                      ]
                  ),
                  ( "Open Positions",
                    Section
                      [ "DataDiscriminator,Asset Category,Currency,Symbol,Quantity,Mult,Cost Price,Cost Basis,Close Price,Value,Unrealized P/L,Unrealized P/L %,Code\n\
                        \Summary,Stocks,USD,VOO,7,1,1.0,1.0,336.55,2000.1,1.0,0.09,"
                      ]
                  ),
                  ( "Trades",
                    Section
                      [ "DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,C. Price,Proceeds,Comm/Fee,Basis,Realized P/L,Realized P/L %,MTM P/L,Code\n\
                        \Order,Stocks,USD,VOO,\"2020-10-05, 09:52:53\",2,309.35,312.08,-618.7,-0.6187,619.3187,0,0,5.46,O;R",
                        "DataDiscriminator,Asset Category,Currency,Symbol,Date/Time,Quantity,T. Price,,Proceeds,Comm in CHF,,,,MTM in CHF,Code\n\
                        \Order,Forex,CHF,USD.CHF,\"2020-01-15, 14:33:56\",\"-2,000,000\",0.96358,,2000000.76,-1.93502,,,,-11,"
                      ]
                  ),
                  ( "Dividends",
                    Section
                      [ "Currency,Date,Description,Amount\n\
                        \USD,2019-12-20,ACWF(US46434V3160) Cash Dividend USD 0.42537 per Share (Ordinary Dividend),1050.24\n\
                        \Total,,,10813.42"
                      ]
                  ),
                  ( "Withholding Tax",
                    Section
                      [ "Currency,Date,Description,Amount,Code\n\
                        \USD,2019-09-06,BND(US9219378356) Cash Dividend USD 0.188198 per Share - US Tax,-2.72\n\
                        \Total,,,-1585.38,"
                      ]
                  ),
                  ( "Deposits & Withdrawals",
                    Section
                      [ "Currency,Settle Date,Description,Amount\n\
                        \CHF,2020-01-20,title,100.32"
                      ]
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
                    [ CashMovement
                        (fromGregorian 2020 1 20)
                        CHF
                        (fromRational (10032 % 100))
                    ],
                  asStockTrades =
                    [ StockTrade (fromGregorian 2020 10 5) "VOO" 2 (fromRational $ -6187 % 10) (fromRational $ -6187 % 10000)
                    ],
                  asForexTrades =
                    [ ForexTrade
                        (fromGregorian 2020 1 15)
                        (QuotePair (BaseCurrency "USD") (QuoteCurrency "CHF"))
                        (-2000000)
                        (fromRational $ 96358 % 100000)
                        (fromRational $ 200000076 % 100)
                        (fromRational $ - (193502 % 100000))
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

      it "Gives a readable error string when taxes can't be parsed" $ do
        let csv =
              Statement
                [ ( "Statement",
                    Section
                      [ "Field Name,Field Value\n\
                        \Period,\"December 11, 2019 - December 4, 2020\"\n"
                      ]
                  ),
                  ("Withholding Tax", Section ["Mangled CSV"])
                ]
        parseActivityStatement csv
          `shouldSatisfy` \case
            Left errorMsg -> (errorMsg =~ "Could not parse Withholding Tax records.")
            Right _ -> False
