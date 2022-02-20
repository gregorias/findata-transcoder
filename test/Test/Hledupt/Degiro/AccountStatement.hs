{-# LANGUAGE OverloadedLists #-}

module Test.Hledupt.Degiro.AccountStatement (
  tests,
) where

import qualified Data.Text as T
import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledger.Read.TestUtils (
  transactionQQ,
 )
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.CsvFile (CsvFile (..))
import Hledupt.Data.Currency (chf, eur)
import Hledupt.Data.Isin (isin)
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Degiro.AccountStatement (csvRecordsToLedger)
import qualified Hledupt.Degiro.AccountStatement as DegiroAccStmt
import Hledupt.Degiro.Csv (DegiroCsvRecord (..), DegiroIsin (DegiroIsin), parseCsvStatement)
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldMatchList, shouldSatisfy)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Degiro.AccountStatement" $ do
    describe "csvRecordsToLedger" csvRecordsToLedgerTests

    it "Parses an account statement" $ do
      let stmt =
            CsvFile $
              encodeUtf8
                [trimming|
                    Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID
                    02-01-2022,00:10,03-01-2022,,,Flatex Interest Income,,EUR,0.00,EUR,-0.00,
                    31-12-2021,11:00,31-12-2021,,,Flatex Interest,,CHF,-0.46,CHF,245.29,|]
      DegiroAccStmt.csvStatementToLedger stmt
        `shouldBe` Right
          ( LedgerReport
              [ [transactionQQ|
                            2021/12/31 * Flatex Interest
                              Assets:Liquid:Degiro  -0.46 CHF = 245.29 CHF
                              Expenses:Financial Services  0.46 CHF|]
              , [transactionQQ|
                            2022/01/02 * Flatex Interest Income
                              Assets:Liquid:Degiro  0.00 EUR = 0 EUR
                              Expenses:Financial Services  0.00 EUR|]
              ]
              []
          )

csvRecordsToLedgerTests :: SpecWith ()
csvRecordsToLedgerTests = do
  it "Returns an empty report for an empty CSV" $ do
    csvRecordsToLedger [] `shouldBe` Right (LedgerReport [] [])

  it "Filters out money market ops" $ do
    let nlIsin = [isin|NL0011280581|]
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          "FUNDSHARE UCITS CHF CASH FUND"
          (Just $ DegiroIsin nlIsin)
          "Money Market fund conversion: Sell 123.5678 at 0.981 CHF"
          Nothing
          Nothing
          (Cash chf 131.72)
          ""
      ]
      `shouldBe` Right (LedgerReport [] [])

  it "Filters out money market fund price changes" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2021 5 10)
          (TimeOfDay 0 0 0)
          (fromGregorian 2021 5 9)
          ""
          Nothing
          "Money Market fund price change (CHF)"
          Nothing
          (Just $ Cash chf (-0.03))
          (Cash chf 3.50)
          ""
      ]
      `shouldBe` Right (LedgerReport [] [])

  it "Parses deposits" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          ""
          Nothing
          "Deposit"
          Nothing
          (Just $ Cash chf 5)
          (Cash chf 5.05)
          ""
      ]
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/09/02 Deposit
                  ! Assets:Liquid:BCGE  -5 CHF
                  * Assets:Liquid:Degiro  5 CHF = 5.05 CHF|]
            ]
            []
        )

  it "Parses withdrawals" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2021 6 20)
          (TimeOfDay 12 33 0)
          (fromGregorian 2021 6 20)
          ""
          Nothing
          "Withdrawal"
          Nothing
          (Just $ Cash chf (-11.28))
          (Cash chf 246.43)
          ""
      ]
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2021/06/20 Deposit
                  ! Assets:Liquid:BCGE    11.28 CHF
                  * Assets:Liquid:Degiro  -11.28 CHF = 246.43 CHF|]
            ]
            []
        )

  it "Parses connection fees" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2022 2 3)
          (TimeOfDay 10 05 0)
          (fromGregorian 2022 1 31)
          ""
          Nothing
          "Giro Exchange Connection Fee 2022"
          Nothing
          (Just $ Cash eur $ -2.50)
          (Cash eur $ -2.50)
          ""
      , DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          ""
          Nothing
          "DEGIRO Exchange Connection Fee 2020 (Euronext Amsterdam - EAM)"
          Nothing
          (Just $ Cash eur $ -2.50)
          (Cash eur $ -2.50)
          ""
      ]
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/09/02 * DEGIRO Exchange Connection Fee 2020 (Euronext Amsterdam - EAM)
                  Assets:Liquid:Degiro  -2.50 EUR = -2.50 EUR
                  Expenses:Financial Services  2.50 EUR|]
            , [transactionQQ|
                2022/02/03 * Giro Exchange Connection Fee 2022
                  Assets:Liquid:Degiro  -2.50 EUR = -2.50 EUR
                  Expenses:Financial Services  2.50 EUR|]
            ]
            []
        )

  it "Parses an Fx transaction" $ do
    Right csvRecords <-
      return $
        parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-02-2020,07:20,03-02-2020,,,FX Debit,,CHF,-2.67,CHF,2382.59,\n\
          \04-02-2020,07:20,03-02-2020,,,FX Credit,0.9351,EUR,2.50,EUR,0.00,\n"
    csvRecordsToLedger csvRecords
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/02/04 * Degiro Forex
                  Assets:Liquid:Degiro  2.50 EUR = 0 EUR
                  Assets:Liquid:Degiro  -2.67 CHF = 2382.59 CHF @ 0.9351 EUR|]
            ]
            []
        )

  it "Parses an unflipped Fx transaction" $ do
    Right csvRecords <-
      return $
        parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \02-09-2020,08:24,01-09-2020,,,FX Debit,,CHF,0.01,CHF,131.72,\n\
          \02-09-2020,08:24,01-09-2020,,,FX Debit,0.9241,EUR,-0.01,EUR,-0.00,"
    csvRecordsToLedger csvRecords
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/09/02 * Degiro Forex
                  Assets:Liquid:Degiro  -0.01 EUR = 0 EUR
                  Assets:Liquid:Degiro  0.01 CHF = 131.72 CHF @ 0.9241 EUR|]
            ]
            []
        )

  it "Parses Fx transactions" $ do
    Right csvRecords <-
      return $
        parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-02-2020,07:20,03-02-2020,,,FX Debit,,CHF,-2.67,CHF,2382.59,\n\
          \04-02-2020,07:20,03-02-2020,,,FX Credit,0.9351,EUR,2.50,EUR,0.00,\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,9733.32,EUR,0.00,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-10406.57,CHF,2385.26,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,11611.68,EUR,-9733.32,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-12414.85,CHF,12791.83,a4457f95-c6b7-43b9-9afa-4410e5109954"
    Right (LedgerReport actualTrs prices) <- return $ csvRecordsToLedger csvRecords
    prices `shouldBe` []
    actualTrs
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

  it "Parses a stock transaction" $ do
    Right csvRecords <-
      return $
        parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \04-03-2020,09:05,04-03-2020,ISHARES MSCI WOR A,IE00B4L5Y983,Buy 659 ISHARES MSCI WOR A@52.845 EUR (IE00B4L5Y983),,EUR,-34824.86,EUR,-34824.86,3a04a583-6cf7-4730-ab8b-31f989bd61fc"
    csvRecordsToLedger csvRecords
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/03/04 * Degiro Stock Transaction
                  Assets:Investments:Degiro:IWDA  659 IWDA @ 52.845 EUR
                  Assets:Liquid:Degiro  -34824.86 EUR = -34824.86 EUR|]
            ]
            []
        )

  -- The statement comes in reverse chronological order.
  -- 'csvRecordsToLedger' should reverse the records.
  it "Parses transactions in correct order" $ do
    Right csvRecords <-
      return $
        parseCsvStatement
          "Date,Time,Value date,Product,ISIN,Description,FX,Change,,Balance,,Order ID\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Credit,0.9353,EUR,9733.32,EUR,0.00,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,FX Debit,,CHF,-10406.57,CHF,2385.26,a4457f95-c6b7-43b9-9afa-4410e5109954\n\
          \03-02-2020,09:05,03-02-2020,ISHARES MSCI WOR A,IE00B4L5Y983,Buy 171 ISHARES MSCI WOR A@56.92 EUR (IE00B4L5Y983),,EUR,-9733.32,EUR,-21345.00,a4457f95-c6b7-43b9-9afa-4410e5109954"
    let ledgerReport = csvRecordsToLedger csvRecords
    ledgerReport
      `shouldBe` Right
        ( LedgerReport
            [ [transactionQQ|
                2020/02/03 * Degiro Stock Transaction
                  Assets:Investments:Degiro:IWDA  171 IWDA @ 56.92 EUR
                  Assets:Liquid:Degiro  -9733.32 EUR = -21345.00 EUR|]
            , [transactionQQ|
                2020/02/03 * Degiro Forex
                  Assets:Liquid:Degiro  -10406.57 CHF = 2385.26 CHF @ 0.9353 EUR
                  Assets:Liquid:Degiro    9733.32 EUR = 0 EUR|]
            ]
            []
        )

  it "Returns a readable error when a record can't be processed." $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          "Bogus commodity"
          Nothing
          "Bogus description"
          Nothing
          Nothing
          (Cash chf 0)
          ""
      ]
      `shouldSatisfy` \case
        Right _ -> False
        Left errMsg ->
          ( "Could not process all elements.\n"
              `T.isInfixOf` errMsg
                && "One remaining row's description: Bogus description"
              `T.isInfixOf` errMsg
          )
