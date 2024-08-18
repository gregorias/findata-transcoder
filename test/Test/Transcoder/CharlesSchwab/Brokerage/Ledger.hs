module Test.Transcoder.CharlesSchwab.Brokerage.Ledger (
  tests,
) where

import Data.Time (fromGregorian)
import Hledger.Extra (showTransaction)
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.HUnit.Extra (assertHead)
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab.Brokerage.Csv (
  BrokerageHistoryCsvRecord (BrokerageHistoryCsvRecord),
 )
import Transcoder.CharlesSchwab.Brokerage.Ledger (brokerageHistoryToLedger)
import Transcoder.CharlesSchwab.DollarAmount (
  DollarAmount (..),
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab.Brokerage.Ledger" $ do
    describe "brokerageHistoryToLedger" $ do
      it "transforms a wire fund entry" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2021 1 19)
                "Wire Funds"
                ""
                "WIRED FUNDS DISBURSED"
                Nothing
                Nothing
                Nothing
                (Just $ DollarAmount (-123.45))
        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                        2021/01/19 Wire Funds
                          * Assets:Liquid:Charles Schwab:Brokerage  -123.45 USD
                          ! Todo|]

      it "transforms a vesting entry" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2020 12 31)
                "Stock Plan Activity"
                "GOOG"
                "ALPHABET INC. CLASS C"
                (Just 5)
                Nothing
                Nothing
                Nothing
        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                  2020/12/31 * GOOG Vesting
                    ! Equity:Charles Schwab:Unvested GOOG  -5 GOOG ; TODO: Check how much of this is coming from dividends.
                    Assets:Investments:Charles Schwab:GOOG  5 GOOG|]

      it "transforms an interest entry" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2021 1 28)
                "Credit Interest"
                ""
                "SCHWAB1 INT 12/30-01/27"
                Nothing
                Nothing
                Nothing
                (Just $ DollarAmount 0.19)

        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                  2021/01/28 * Credit Interest
                    Assets:Liquid:Charles Schwab:Brokerage  0.19 USD
                    Income:Google|]
      it "transforms a sell entry (2020)" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2020 12 31)
                "Sell"
                "GOOG"
                "ALPHABET INC. CLASS C"
                (Just 8)
                (Just $ DollarAmount 1765.2706)
                (Just $ DollarAmount 0.31)
                (Just $ DollarAmount 14121.85)
        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                  2020/12/31 * GOOG Sale
                    Assets:Investments:Charles Schwab:GOOG  -8 GOOG @ 1765.2706 USD
                    Assets:Liquid:Charles Schwab:Brokerage  USD 14121.85
                    Expenses:Financial Services             USD     0.31|]
      it "transforms a sell entry (2024)" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2024 06 27)
                "Sell"
                "GOOG"
                "ALPHABET INC. CLASS C"
                (Just 40.045)
                (Just $ DollarAmount 185.2337)
                (Just $ DollarAmount 0.22)
                (Just $ DollarAmount 7417.46)
        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                  2024/06/27 * GOOG Sale
                    Assets:Investments:Charles Schwab:GOOG  GOOG -40.045 @ USD 185.2337
                    Assets:Liquid:Charles Schwab:Brokerage  USD 7417.46
                    Expenses:Financial Services             USD    0.22|]
      it "transforms a withholding tax entry" $ do
        let entry =
              BrokerageHistoryCsvRecord
                (fromGregorian 2020 12 31)
                "Journal"
                ""
                "Gencash transaction for SPS RS Lapse Tool"
                Nothing
                Nothing
                Nothing
                (Just $ DollarAmount (-1.23))
        brokerageHistoryToLedger [entry]
          `shouldBe` [transactionsQQ|
                       2020/12/31 * Withholding Tax
                         Assets:Liquid:Charles Schwab:Brokerage  -1.23 USD
                         Equity:Charles Schwab:Unvested GOOG Withholding Tax|]

      describe "supports fractional GOOG printing" $ do
        it "in sell entries" $ do
          let entry =
                BrokerageHistoryCsvRecord
                  (fromGregorian 2024 06 27)
                  "Sell"
                  "GOOG"
                  "ALPHABET INC. CLASS C"
                  (Just 40.045)
                  (Just $ DollarAmount 185.2337)
                  (Just $ DollarAmount 0.22)
                  (Just $ DollarAmount 7417.46)
          tr <- assertHead $ brokerageHistoryToLedger [entry]
          showTransaction tr
            `shouldBe` "2024-06-27 * GOOG Sale\n\
                       \    Assets:Investments:Charles Schwab:GOOG    GOOG -40.045 @ USD 185.2337\n\
                       \    Assets:Liquid:Charles Schwab:Brokerage                    USD 7417.46\n\
                       \    Expenses:Financial Services                                  USD 0.22\n"
