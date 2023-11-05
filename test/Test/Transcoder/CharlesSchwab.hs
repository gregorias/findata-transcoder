module Test.Transcoder.CharlesSchwab (tests) where

import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.HUnit.Extra (assertRight)
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec qualified as Hspec
import Transcoder.CharlesSchwab as CS

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab" $ do
    describe "csvToLedger" $ do
      it "converts to a report" $ do
        csv <- readFileLBS "test/data/cs-brokerage-account-history.csv"
        trs <- assertRight . CS.parseBrokerageAccountHistory $ csv
        trs
          `shouldBe` [transactionsQQ|
            2022/09/29 * GOOG Sale
              Assets:Investments:Charles Schwab:GOOG    GOOG -40 @ USD 98.5606
              Assets:Liquid:Charles Schwab:USD                     USD 3942.33
              Expenses:Financial Services                             USD 0.09

            2022/09/29 * Withholding Tax
              Assets:Liquid:Charles Schwab:USD     USD  -1538.33
              Equity:Charles Schwab:Unvested GOOG Withholding Tax

            2022/09/30 Wire Sent
              * Assets:Liquid:Charles Schwab:USD   USD -10818.38
              ! ToDo

            2022/10/28 * Credit Interest
              Assets:Liquid:Charles Schwab:USD     USD      0.07
              Income:Google|]
