{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.CharlesSchwab.Ledger (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.CharlesSchwab.Csv (
  CsCsvRecord (CsCsvRecord),
  DollarAmount (..),
 )
import Hledupt.CharlesSchwab.Ledger (csvToLedger)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.CharlesSchwab.Ledger" $ do
    describe "csvToLedger" $ do
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
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2021/01/19 Wire Funds\n\
                    \  * Assets:Liquid:Charles Schwab:USD  -123.45 USD\n\
                    \  ! Todo"
                ]
                []
            )
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
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2020/12/31 * GOOG Vesting\n\
                    \  Assets:Illiquid:Charles Schwab:Unvested GOOG  -5 GOOG\n\
                    \  Assets:Investments:Charles Schwab:GOOG  5 GOOG"
                ]
                []
            )
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
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2021/01/28 * Credit Interest\n\
                    \  Assets:Liquid:Charles Schwab:USD  0.19 USD\n\
                    \  Income:Google"
                ]
                []
            )
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
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2020/12/31 * GOOG Sale\n\
                    \  Assets:Investments:Charles Schwab:GOOG  -8 GOOG @ 1765.2706 USD\n\
                    \  Assets:Liquid:Charles Schwab:USD  14121.85 USD\n\
                    \  Expenses:Financial Services  0.31 USD"
                ]
                []
            )
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
                  (Just $ DollarAmount (fromRational $ 123 % 100))
              ]
        csvToLedger entries
          `shouldBe` Right
            ( LedgerReport
                [ parseTransactionUnsafe
                    "2020/12/31 * Withholding Tax\n\
                    \  Assets:Liquid:Charles Schwab:USD  -1.23 USD\n\
                    \  Taxes"
                ]
                []
            )
