module Test.Transcoder.Revolut.Csv (
  tests,
) where

import Data.Time (fromGregorian)
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import Transcoder.Data.Currency (chf, eur)
import Transcoder.Revolut.Csv (
  CompletedTransaction (..),
  RevertedTransaction (..),
  Transaction (..),
  TransactionType (..),
  parse,
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.Revolut.Csv" $ do
    describe "parse" $ do
      it "Parses a completed transaction" $ do
        let csv =
              CsvFile
                . encodeUtf8
                $ [trimming|
            Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance
            TOPUP,Current,2021-08-11 08:04:22,2021-08-11 08:04:42,Top-Up by *7817,15.00,0.00,CHF,COMPLETED,23.95|]
        parse csv
          `shouldBe` Right
            [ TransactionCompletedTransaction
                ( CompletedTransaction
                    Topup
                    (fromGregorian 2021 8 11)
                    (fromGregorian 2021 8 11)
                    "Top-Up by *7817"
                    15.00
                    0
                    chf
                    23.95
                )
            ]

      it "Parses a reverted transaction" $ do
        let csv =
              CsvFile
                . encodeUtf8
                $ [trimming|
            Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance
            CARD_PAYMENT,Current,2025-03-21 06:43:41,,Cablemod,-77.91,0.00,EUR,REVERTED,|]
        parse csv
          `shouldBe` Right
            [ TransactionRevertedTransaction
                ( RevertedTransaction
                    (fromGregorian 2025 3 21)
                    "Cablemod"
                    (-77.91)
                    0
                    eur
                )
            ]
