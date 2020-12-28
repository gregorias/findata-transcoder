{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Degiro.Transactions (
  tests,
) where

import qualified Data.Text as Text
import Data.Time (fromGregorian)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.Currency (Currency (..))
import Hledupt.Data.Isin (mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Degiro.Csv (
  DegiroCsvRecord (..),
  parseCsvStatement,
 )
import Hledupt.Degiro.Transactions (csvRecordsToLedger)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe, shouldMatchList, shouldSatisfy)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Degiro" $ do
    describe "csvRecordsToLedger" csvRecordsToLedgerTests

csvRecordsToLedgerTests :: SpecWith ()
csvRecordsToLedgerTests = do
  it "Returns an empty report for an empty CSV" $ do
    csvRecordsToLedger [] `shouldBe` Right (LedgerReport [] [])

  it "Filters out money market ops" $ do
    Just nlIsin <- return $ mkIsin "NL0011280581"
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          "FUNDSHARE UCITS CHF CASH FUND"
          (Just nlIsin)
          "Money Market fund conversion: Sell 123.5678 at 0.981 CHF"
          Nothing
          Nothing
          (Cash CHF 131.72)
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
          (Just $ Cash CHF 5)
          (Cash CHF 5.05)
          ""
      ]
      `shouldBe` Right
        ( LedgerReport
            [ parseTransactionUnsafe
                "2020/09/02 Deposit\n\
                \  ! Assets:Liquid:BCGE  -5 CHF\n\
                \  * Assets:Liquid:Degiro  5 CHF = 5.05 CHF"
            ]
            []
        )

  it "Parses connection fees" $ do
    csvRecordsToLedger
      [ DegiroCsvRecord
          (fromGregorian 2020 9 2)
          (TimeOfDay 12 02 0)
          (fromGregorian 2020 9 1)
          ""
          Nothing
          "DEGIRO Exchange Connection Fee 2020 (Euronext Amsterdam - EAM)"
          Nothing
          (Just $ Cash EUR $ -2.50)
          (Cash EUR $ -2.50)
          ""
      ]
      `shouldBe` Right
        ( LedgerReport
            [ parseTransactionUnsafe
                "2020/09/02 * Exchange Connection Fee\n\
                \  Assets:Liquid:Degiro  -2.50 EUR = -2.50 EUR\n\
                \  Expenses:Financial Services  2.50 EUR"
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
            [ parseTransactionUnsafe
                "2020/02/04 * Degiro Forex\n\
                \  Assets:Liquid:Degiro  2.50 EUR = 0 EUR\n\
                \  Assets:Liquid:Degiro  -2.67 CHF = 2382.59 CHF @ 0.9351 EUR\n"
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
            [ parseTransactionUnsafe
                "2020/09/02 * Degiro Forex\n\
                \  Assets:Liquid:Degiro  -0.01 EUR = 0 EUR\n\
                \  Assets:Liquid:Degiro  0.01 CHF = 131.72 CHF @ 0.9241 EUR\n"
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
      `shouldMatchList` [ parseTransactionUnsafe
                            "2020/02/03 * Degiro Forex\n\
                            \  Assets:Liquid:Degiro  -12414.85 CHF = 12791.83 CHF @ 0.9353 EUR\n\
                            \  Assets:Liquid:Degiro  11611.68 EUR = -9733.32 EUR\n"
                        , parseTransactionUnsafe
                            "2020/02/03 * Degiro Forex\n\
                            \  Assets:Liquid:Degiro  -10406.57 CHF = 2385.26 CHF @ 0.9353 EUR\n\
                            \  Assets:Liquid:Degiro    9733.32 EUR = 0 EUR\n"
                        , parseTransactionUnsafe
                            "2020/02/04 * Degiro Forex\n\
                            \  Assets:Liquid:Degiro  2.50 EUR = 0 EUR\n\
                            \  Assets:Liquid:Degiro  -2.67 CHF = 2382.59 CHF @ 0.9351 EUR\n"
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
            [ parseTransactionUnsafe
                "2020/03/04 * Degiro Stock Transaction\n\
                \  Assets:Investments:Degiro:IWDA  659 IWDA @ 52.845 EUR\n\
                \  Assets:Liquid:Degiro  -34824.86 EUR = -34824.86 EUR"
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
    Right (LedgerReport actualTrs prices) <- return $ csvRecordsToLedger csvRecords
    prices `shouldBe` []
    actualTrs
      `shouldBe` [ parseTransactionUnsafe
                    "2020/02/03 * Degiro Stock Transaction\n\
                    \  Assets:Investments:Degiro:IWDA  171 IWDA @ 56.92 EUR\n\
                    \  Assets:Liquid:Degiro  -9733.32 EUR = -21345.00 EUR"
                 , parseTransactionUnsafe
                    "2020/02/03 * Degiro Forex\n\
                    \  Assets:Liquid:Degiro  -10406.57 CHF = 2385.26 CHF @ 0.9353 EUR\n\
                    \  Assets:Liquid:Degiro    9733.32 EUR = 0 EUR\n"
                 ]

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
          (Cash CHF 0)
          ""
      ]
      `shouldSatisfy` \case
        Right _ -> False
        Left errMsg ->
          ( "Could not process all elements.\n"
              `Text.isInfixOf` errMsg
                && "One remaining row's description: Bogus description"
              `Text.isInfixOf` errMsg
          )
