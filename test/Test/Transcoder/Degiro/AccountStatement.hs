{-# LANGUAGE OverloadedLists #-}

module Test.Transcoder.Degiro.AccountStatement (
  tests,
) where

import Data.Cash (Cash (Cash))
import Data.Text qualified as T
import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledger.Read.TestUtils (
  transactionQQ,
 )
import NeatInterpolation (trimming)
import Relude
import Test.HUnit.Extra (assertLeft, assertRight)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldMatchList, shouldSatisfy)
import Transcoder.Data.CsvFile (CsvFile (..))
import Transcoder.Data.Currency (chf, eur)
import Transcoder.Data.Isin (isin)
import Transcoder.Degiro.AccountStatement (csvRecordsToLedger)
import Transcoder.Degiro.AccountStatement qualified as DegiroAccStmt
import Transcoder.Degiro.Csv (DegiroCsvRecord (..), DegiroIsin (DegiroIsin), parseCsvStatement)

tests :: SpecWith ()
tests = do
  describe "Transcoder.Degiro.AccountStatement" $ do
    describe "csvRecordsToLedger" csvRecordsToLedgerTests

    it "Parses a Flatex Interest statement" $ do
      let stmt =
            CsvFile
              $ encodeUtf8
                [trimming|
                    Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID
                    02-01-2022,00:10,03-01-2022,,,Flatex Interest Income,,EUR,0.00,EUR,-0.00,
                    31-12-2021,11:00,31-12-2021,,,Flatex Interest,,CHF,-0.46,CHF,245.29,|]
      DegiroAccStmt.csvStatementToLedger stmt
        `shouldBe` Right
          [ [transactionQQ|
              2021/12/31 * Flatex Interest
                Assets:Liquid:Degiro        -0.46 CHF = 245.29 CHF
                Expenses:Financial Services  0.46 CHF|]
          , [transactionQQ|
              2022/01/02 * Flatex Interest Income
                Assets:Liquid:Degiro         0.00 EUR = 0 EUR
                Expenses:Financial Services  0.00 EUR|]
          ]

csvRecordsToLedgerTests :: SpecWith ()
csvRecordsToLedgerTests = do
  it "Returns an empty report for an empty CSV" $ do
    csvRecordsToLedger [] `shouldBe` Right []

  it "Filters out money market ops" $ do
    let nlIsin = [isin|NL0011280581|]
    csvRecordsToLedger
      [ DegiroCsvRecord
          { dcrDate = fromGregorian 2020 9 2
          , dcrTime = TimeOfDay 12 02 0
          , dcrValueDate = fromGregorian 2020 9 1
          , dcrProduct = "FUNDSHARE UCITS CHF CASH FUND"
          , dcrIsin = Just $ DegiroIsin nlIsin
          , dcrDescription = "Money Market fund conversion: Sell 123.5678 at 0.981 CHF"
          , dcrFx = Nothing
          , dcrChange = Nothing
          , dcrBalance = Cash chf 131.72
          , dcrOrderId = ""
          }
      ]
      `shouldBe` Right []

  it "Filters out money market fund price changes" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          { dcrDate = fromGregorian 2021 5 10
          , dcrTime = TimeOfDay 0 0 0
          , dcrValueDate = fromGregorian 2021 5 9
          , dcrProduct = ""
          , dcrIsin = Nothing
          , dcrDescription = "Money Market fund price change (CHF)"
          , dcrFx = Nothing
          , dcrChange = Just $ Cash chf (-0.03)
          , dcrBalance = Cash chf 3.50
          , dcrOrderId = ""
          }
      ]
      `shouldBe` Right []

  it "Parses deposits" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          { dcrDate = fromGregorian 2020 9 2
          , dcrTime = TimeOfDay 12 02 0
          , dcrValueDate = fromGregorian 2020 9 1
          , dcrProduct = ""
          , dcrIsin = Nothing
          , dcrDescription = "Deposit"
          , dcrFx = Nothing
          , dcrChange = Just $ Cash chf 5
          , dcrBalance = Cash chf 5.05
          , dcrOrderId = ""
          }
      ]
      `shouldBe` Right
        [ [transactionQQ|
                2020/09/02 Deposit
                  ! Assets:Liquid:BCGE  -5 CHF
                  * Assets:Liquid:Degiro  5 CHF = 5.05 CHF|]
        ]

  it "Parses withdrawals" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          { dcrDate = fromGregorian 2021 6 20
          , dcrTime = TimeOfDay 12 33 0
          , dcrValueDate = fromGregorian 2021 6 20
          , dcrProduct = ""
          , dcrIsin = Nothing
          , dcrDescription = "Withdrawal"
          , dcrFx = Nothing
          , dcrChange = Just $ Cash chf (-11.28)
          , dcrBalance = Cash chf 246.43
          , dcrOrderId = ""
          }
      ]
      `shouldBe` Right
        [ [transactionQQ|
                2021/06/20 Deposit
                  ! Assets:Liquid:BCGE    11.28 CHF
                  * Assets:Liquid:Degiro  -11.28 CHF = 246.43 CHF|]
        ]

  it "Parses connection fees" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          { dcrDate = fromGregorian 2022 2 3
          , dcrTime = TimeOfDay 10 05 0
          , dcrValueDate = fromGregorian 2022 1 31
          , dcrProduct = ""
          , dcrIsin = Nothing
          , dcrDescription = "Giro Exchange Connection Fee 2022"
          , dcrFx = Nothing
          , dcrChange = Just $ Cash eur $ -2.50
          , dcrBalance = Cash eur $ -2.50
          , dcrOrderId = ""
          }
      , DegiroCsvRecord
          { dcrDate = fromGregorian 2020 9 2
          , dcrTime = TimeOfDay 12 02 0
          , dcrValueDate = fromGregorian 2020 9 1
          , dcrProduct = ""
          , dcrIsin = Nothing
          , dcrDescription = "DEGIRO Exchange Connection Fee 2020 (Euronext Amsterdam - EAM)"
          , dcrFx = Nothing
          , dcrChange = Just $ Cash eur $ -2.50
          , dcrBalance = Cash eur $ -2.50
          , dcrOrderId = ""
          }
      ]
      `shouldBe` Right
        [ [transactionQQ|
                2020/09/02 * DEGIRO Exchange Connection Fee 2020 (Euronext Amsterdam - EAM)
                  Assets:Liquid:Degiro  -2.50 EUR = -2.50 EUR
                  Expenses:Financial Services  2.50 EUR|]
        , [transactionQQ|
                2022/02/03 * Giro Exchange Connection Fee 2022
                  Assets:Liquid:Degiro  -2.50 EUR = -2.50 EUR
                  Expenses:Financial Services  2.50 EUR|]
        ]

  it "Parses a cash account transfer." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
          [ DegiroCsvRecord
              { dcrDate = fromGregorian 2022 2 7
              , dcrTime = TimeOfDay 10 17 0
              , dcrValueDate = fromGregorian 2022 2 4
              , dcrProduct = ""
              , dcrIsin = Nothing
              , dcrDescription = "Transfer from your Cash Account at flatex Bank: 2.63 CHF"
              , dcrFx = Nothing
              , dcrChange = Nothing
              , dcrBalance = Cash chf 242.66
              , dcrOrderId = ""
              }
          ]
    ledger
      `shouldBe` [ [transactionQQ|
            2022/02/07 * Transfer from your Cash Account at flatex Bank: 2.63 CHF
              Assets:Liquid:Degiro  -2.63 CHF = 242.66 CHF
              Expenses:Financial Services  2.63 CHF|]
                 ]

  it "Parses an Fx transaction." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
        =<< parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-02-2020,07:20,03-02-2020,,,FX Debit,,CHF,-2.67,CHF,2382.59,\n\
          \04-02-2020,07:20,03-02-2020,,,FX Credit,0.9351,EUR,2.50,EUR,0.00,\n"
    ledger
      `shouldBe` [ [transactionQQ|
            2020/02/04 * Degiro Forex
              Assets:Liquid:Degiro  2.50 EUR = 0 EUR
              Assets:Liquid:Degiro  -2.67 CHF = 2382.59 CHF @ 0.9351 EUR|]
                 ]

  it "Parses an unflipped Fx transaction." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
        =<< parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \02-09-2020,08:24,01-09-2020,,,FX Debit,,CHF,0.01,CHF,131.72,\n\
          \02-09-2020,08:24,01-09-2020,,,FX Debit,0.9241,EUR,-0.01,EUR,-0.00,"
    ledger
      `shouldBe` [ [transactionQQ|
            2020/09/02 * Degiro Forex
              Assets:Liquid:Degiro  -0.01 EUR = 0 EUR
              Assets:Liquid:Degiro  0.01 CHF = 131.72 CHF @ 0.9241 EUR|]
                 ]

  it "Parses Fx transactions." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
        =<< parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-02-2020,07:20,03-02-2020,,,FX Debit,,CHF,-2.67,CHF,2382.59,\n\
          \04-02-2020,07:20,03-02-2020,,,FX Credit,0.9351,EUR,2.50,EUR,0.00,\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,9733.32,EUR,0.00,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-10406.57,CHF,2385.26,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,11611.68,EUR,-9733.32,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-12414.85,CHF,12791.83,a4457f95-c6b7-43b9-9afa-4410e5109954"
    ledger
      `shouldMatchList` [ [transactionQQ|
                            2020/02/03 * Degiro Forex
                              Assets:Liquid:Degiro  -12414.85 CHF = 12791.83 CHF @ 0.9353 EUR
                              Assets:Liquid:Degiro  11611.68 EUR = -9733.32 EUR|]
                        , [transactionQQ|
                            2020/02/03 * Degiro Forex
                              Assets:Liquid:Degiro  -10406.57 CHF = 2385.26 CHF @ 0.9353 EUR
                              Assets:Liquid:Degiro    9733.32 EUR = 0 EUR|]
                        , [transactionQQ|
                            2020/02/04 * Degiro Forex
                              Assets:Liquid:Degiro  2.50 EUR = 0 EUR
                              Assets:Liquid:Degiro  -2.67 CHF = 2382.59 CHF @ 0.9351 EUR|]
                        ]

  it "Parses a stock transaction." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
        =<< parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-03-2020,09:05,04-03-2020,ISHARES MSCI WOR A,IE00B4L5Y983,Buy 659 ISHARES MSCI WOR A@52.845 EUR (IE00B4L5Y983),,EUR,-34824.86,EUR,-34824.86,3a04a583-6cf7-4730-ab8b-31f989bd61fc"
    ledger
      `shouldBe` [ [transactionQQ|
                2020/03/04 * Degiro Stock Transaction
                  Assets:Investments:Degiro:IWDA  659 IWDA @ 52.845 EUR
                  Assets:Liquid:Degiro  -34824.86 EUR = -34824.86 EUR|]
                 ]

  -- The statement comes in reverse chronological order.
  -- 'csvRecordsToLedger' should reverse the records.
  it "Parses transactions in correct order." $ do
    ledger <-
      assertRight
        $ csvRecordsToLedger
        =<< parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,9733.32,EUR,0.00,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-10406.57,CHF,2385.26,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,Buy 171 ISHARES MSCI WOR A@56.92 EUR (IE00B4L5Y983),,EUR,-9733.32,EUR,-21345.00,a4457f95-c6b7-43b9-9afa-4410e5109954"
    ledger
      `shouldBe` [ [transactionQQ|
                2020/02/03 * Degiro Stock Transaction
                  Assets:Investments:Degiro:IWDA  171 IWDA @ 56.92 EUR
                  Assets:Liquid:Degiro  -9733.32 EUR = -21345.00 EUR|]
                 , [transactionQQ|
                2020/02/03 * Degiro Forex
                  Assets:Liquid:Degiro  -10406.57 CHF = 2385.26 CHF @ 0.9353 EUR
                  Assets:Liquid:Degiro    9733.32 EUR = 0 EUR|]
                 ]

  it "Returns a readable error when a record can't be processed."
    $ do
      errorMsg <-
        assertLeft
          $ csvRecordsToLedger
            [ DegiroCsvRecord
                { dcrDate = fromGregorian 2020 9 2
                , dcrTime = TimeOfDay 12 02 0
                , dcrValueDate = fromGregorian 2020 9 1
                , dcrProduct = "Bogus commodity"
                , dcrIsin = Nothing
                , dcrDescription = "Bogus description"
                , dcrFx = Nothing
                , dcrChange = Nothing
                , dcrBalance = Cash chf 0
                , dcrOrderId = ""
                }
            ]
      errorMsg
        `shouldSatisfy` ( \errMsg ->
                            "Could not parse CSV record into a smart record: Bogus description"
                              `T.isInfixOf` errMsg
                        )
