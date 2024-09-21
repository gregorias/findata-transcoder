module Test.Transcoder.CharlesSchwab (tests) where

import Data.ByteString qualified as BS
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.HUnit.Extra (assertLeft, assertRight, assertRightOrFailPrint, textShouldContain)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab as CS

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab" $ do
    describe "parseBrokerageAccountHistory" $ do
      it "converts a CSV file to a report (2024-08)" $ do
        csv <- readFileLBS "test/data/cs-brokerage-account-history-2024-08.csv"
        trs <- assertRight . CS.parseBrokerageAccountHistory $ csv
        trs
          `shouldBe` [transactionsQQ|
            2024/06/27 * GOOG Sale
              Assets:Investments:Charles Schwab:GOOG  GOOG -40.045 @ USD 185.2337
              Assets:Liquid:Charles Schwab:Brokerage  USD 7417.46
              Expenses:Financial Services             USD    0.22

            2024/06/27 * GOOG Vesting
              ! Equity:Charles Schwab:Unvested GOOG     GOOG -40.045 ; TODO: Check how much of this is coming from dividends.
              Assets:Investments:Charles Schwab:GOOG  GOOG  40.045

            2024/06/27 * GOOG Sale
              Assets:Investments:Charles Schwab:GOOG  GOOG -21.024 @ USD 185.2337
              Assets:Liquid:Charles Schwab:Brokerage  USD 3894.24
              Expenses:Financial Services             USD    0.11

            2024/06/27 * GOOG Vesting
              ! Equity:Charles Schwab:Unvested GOOG     GOOG -21.024 ; TODO: Check how much of this is coming from dividends.
              Assets:Investments:Charles Schwab:GOOG  GOOG  21.024

            2024/06/27 * Withholding Tax
              Assets:Liquid:Charles Schwab:Brokerage  USD -1438.65
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2024/06/27 * Withholding Tax
              Assets:Liquid:Charles Schwab:Brokerage  USD -2740.24
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2024/06/27 * Credit Interest
              Assets:Liquid:Charles Schwab:Brokerage  USD 0.84
              Income:Google

            2024/06/28 Wire Sent
              * Assets:Liquid:Charles Schwab:Brokerage  USD -14139.28
              ! Todo

            2024/07/29 * GOOG Sale
              Assets:Investments:Charles Schwab:GOOG  GOOG  -22.025 @ USD 168.1593
              Assets:Liquid:Charles Schwab:Brokerage   USD  3703.61
              Expenses:Financial Services              USD     0.10

            2024/07/29 * GOOG Vesting
              ! Equity:Charles Schwab:Unvested GOOG  GOOG   -22.025 ; TODO: Check how much of this is coming from dividends.
              Assets:Investments:Charles Schwab:GOOG  GOOG   22.025

            2024/07/29 * Withholding Tax
              Assets:Liquid:Charles Schwab:Brokerage  USD -1453.63
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2024/07/30 * Credit Interest
              Assets:Liquid:Charles Schwab:Brokerage  USD  0.20
              Income:Google

            2024/07/31 Wire Sent
              * Assets:Liquid:Charles Schwab:Brokerage  USD -2250.18
              ! Todo|]

    describe "parseEacAccountHistory" $ do
      it "prints a helpful error message if the input is not a JSON object" $ do
        let csv = "Date,Description,Amount\n2023-06-28,GOOG Deposit,12.245\n"
        errorMsg <- assertLeft $ CS.parseEacAccountHistory csv
        errorMsg
          `textShouldContain` ( "Could not decode the input as a valid JSON object."
                                  <> " EAC account history needs to be in the JSON format.\n"
                              )

      it "converts a JSON statement to a report" $ do
        json <- BS.readFile "test/data/cs-eac-account-history.json"
        trs <- assertRightOrFailPrint $ CS.parseEacAccountHistory json
        trs
          `shouldBe` [transactionsQQ|
            2023/06/28 * GOOG Deposit
              Assets:Investments:Charles Schwab:EAC  GOOG  12.245
              ! Equity:Charles Schwab:Unvested GOOG    GOOG -12.245 ; TODO: Check state on https://client.schwab.com/app/accounts/equityawards/#/ and add a balance assertion
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2023/07/27 * GOOG Deposit
              Assets:Investments:Charles Schwab:EAC  GOOG  13.671
              ! Equity:Charles Schwab:Unvested GOOG    GOOG -13.671 ; TODO: Check state on https://client.schwab.com/app/accounts/equityawards/#/ and add a balance assertion
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2023/08/01 * GOOG Sale
              Assets:Investments:Charles Schwab:EAC  GOOG  -150.806
              Assets:Liquid:Charles Schwab:EAC:USD   USD  19717.70
              Expenses:Financial Services            USD      0.18

            2023/08/04 * Wire Transfer
              Assets:Liquid:Charles Schwab:EAC:USD   USD -19717.70
              ! Todo
            |]
