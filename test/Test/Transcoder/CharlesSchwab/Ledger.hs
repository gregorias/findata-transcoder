{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Transcoder.CharlesSchwab.Ledger (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.HUnit.Extra (assertRight)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord),
  DollarAmount (..),
 )
import Transcoder.CharlesSchwab.Ledger (brokerageHistoryToLedger)

tests :: SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab.Ledger" $ do
    describe "brokerageHistoryToLedger" $ do
      it "transforms a wire fund entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2021 1 19)
                  "Wire Funds"
                  ""
                  "WIRED FUNDS DISBURSED"
                  Nothing
                  Nothing
                  Nothing
                  (Just $ DollarAmount (fromRational $ -12345 % 100))
              ]
        trs <- assertRight $ brokerageHistoryToLedger entries
        trs
          `shouldBe` [transactionsQQ|
                        2021/01/19 Wire Funds
                          * Assets:Liquid:Charles Schwab:USD  -123.45 USD
                          ! Todo|]

      it "transforms a vesting entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2020 12 31)
                  "Stock Plan Activity"
                  "GOOG"
                  "ALPHABET INC. CLASS C"
                  (Just 5)
                  Nothing
                  Nothing
                  Nothing
              ]
        trs <- assertRight $ brokerageHistoryToLedger entries
        trs
          `shouldBe` [transactionsQQ|
                  2020/12/31 * GOOG Vesting
                    Equity:Charles Schwab:Unvested GOOG  -5 GOOG
                    Assets:Investments:Charles Schwab:GOOG  5 GOOG|]

      it "transforms an interest entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2021 1 28)
                  "Credit Interest"
                  ""
                  "SCHWAB1 INT 12/30-01/27"
                  Nothing
                  Nothing
                  Nothing
                  (Just $ DollarAmount (fromRational $ 19 % 100))
              ]
        trs <- assertRight $ brokerageHistoryToLedger entries
        trs
          `shouldBe` [transactionsQQ|
                  2021/01/28 * Credit Interest
                    Assets:Liquid:Charles Schwab:USD  0.19 USD
                    Income:Google|]
      it "transforms an sell entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2020 12 31)
                  "Sell"
                  "GOOG"
                  "ALPHABET INC. CLASS C"
                  (Just 8)
                  (Just $ DollarAmount (fromRational $ 17652706 % 10000))
                  (Just $ DollarAmount (fromRational $ 31 % 100))
                  (Just $ DollarAmount (fromRational $ 1412185 % 100))
              ]
        trs <- assertRight $ brokerageHistoryToLedger entries
        trs
          `shouldBe` [transactionsQQ|
                  2020/12/31 * GOOG Sale
                    Assets:Investments:Charles Schwab:GOOG  -8 GOOG @ 1765.2706 USD
                    Assets:Liquid:Charles Schwab:USD  14121.85 USD
                    Expenses:Financial Services  0.31 USD|]
      it "transforms a withholding tax entry" $ do
        let entries =
              [ CsCsvRecord
                  (fromGregorian 2020 12 31)
                  "Journal"
                  ""
                  "Gencash transaction for SPS RS Lapse Tool"
                  Nothing
                  Nothing
                  Nothing
                  (Just $ DollarAmount (fromRational $ -123 % 100))
              ]
        trs <- assertRight $ brokerageHistoryToLedger entries
        trs
          `shouldBe` [transactionsQQ|
                       2020/12/31 * Withholding Tax
                         Assets:Liquid:Charles Schwab:USD  -1.23 USD
                         Equity:Charles Schwab:Unvested GOOG Withholding Tax|]
