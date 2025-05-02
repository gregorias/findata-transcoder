module Test.Transcoder.Revolut (
  tests,
) where

import Hledger.Read.TestUtils (transactionsQQ)
import NeatInterpolation (trimming)
import Relude
import Test.HUnit.Extra (assertLeft)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec qualified as Hspec
import Transcoder.Data.LedgerReport (LedgerReport (..))
import Transcoder.Revolut qualified as Revolut

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Revolut" $ do
    it "gives a useful error message on a faulty field" $ do
      let csv =
            fromString
              . toString
              $ [trimming|
           Type,Product,Started Date,Completed Date,Description,Amount,Fee,Currency,State,Balance
           CARD_PAYMENT,Current,2025-03-21 06:43:41,,Cablemod,,0.00,EUR,REVERTED,|]
      message <- assertLeft $ Revolut.parseCsvToLedger csv
      message `shouldBe` "parse error (Failed reading: conversion error: Couldn't parse {Amount:,Description:Cablemod,Fee:0.00,Balance:,Started Date:2025-03-21 06:43:41,State:REVERTED,Currency:EUR,Product:Current,Type:CARD_PAYMENT,Completed Date:}\nin named field \"Amount\": Could not parse the string \"\" as a decimal.) at \"\""

    it "parses a CSV 2021 report to a LedgerReport" $ do
      csv <- readFileLBS "test/data/revolut0.csv"
      Revolut.parseCsvToLedger csv
        `shouldBe` Right
          ( LedgerReport
              [transactionsQQ|
                2021/08/11 * Top-Up by *7817
                  Assets:Liquid:Revolut:CHF  CHF 15 = CHF 23.95
                  ! Todo
                2021/08/18 * Amzn Mktp De
                  Assets:Liquid:Revolut:CHF  CHF -28.61 = CHF 104.43
                  ! Todo
                2021/08/20 * To John Doe
                  Assets:Liquid:Revolut:CHF  CHF -11.62 = CHF 92.81
                  ! Todo
                2021/08/26 * Exchanged to PLN
                  Assets:Liquid:Revolut:CHF  CHF -86.05 = CHF 0
                  ! Todo|]
              []
          )

    it "parses a CSV 2025 report with a reversion to a LedgerReport" $ do
      csv <- readFileLBS "test/data/revolut1.csv"
      Revolut.parseCsvToLedger csv
        `shouldBe` Right
          ( LedgerReport
              [transactionsQQ|
                2025/03/16 * Amazon
                  Assets:Liquid:Revolut:EUR  EUR -9.38 = EUR 134.90
                  ! Todo
                2025/03/21 * Cablemod
                  Assets:Liquid:Revolut:EUR  EUR 77.91
                  ! Todo|]
              []
          )
