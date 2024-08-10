module Test.Transcoder.CharlesSchwab (tests) where

import Data.ByteString qualified as BS
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.HUnit.Extra (assertRight, assertRightOrFailPrint)
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.CharlesSchwab as CS

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab" $ do
    describe "parseBrokerageAccountHistory" $ do
      it "converts a CSV file to a report" $ do
        csv <- readFileLBS "test/data/cs-brokerage-account-history.csv"
        trs <- assertRight . CS.parseBrokerageAccountHistory $ csv
        trs
          `shouldBe` [transactionsQQ|
            2022/09/29 * GOOG Sale
              Assets:Investments:Charles Schwab:GOOG    GOOG -40 @ USD 98.5606
              Assets:Liquid:Charles Schwab:Brokerage                     USD 3942.33
              Expenses:Financial Services                             USD 0.09

            2022/09/29 * Withholding Tax
              Assets:Liquid:Charles Schwab:Brokerage     USD  -1538.33
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2022/09/30 Wire Sent
              * Assets:Liquid:Charles Schwab:Brokerage   USD -10818.38
              ! Todo

            2022/10/28 * Credit Interest
              Assets:Liquid:Charles Schwab:Brokerage     USD      0.07
              Income:Google|]

    describe "parseEacAccountHistory" $ do
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
