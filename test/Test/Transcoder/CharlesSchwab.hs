module Test.Transcoder.CharlesSchwab (tests) where

import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Transcoder.CharlesSchwab as CS
import Transcoder.Data.LedgerReport (LedgerReport (..))

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.CharlesSchwab" $ do
    describe "csvToLedger" $ do
      it "converts to a report" $ do
        csv <- readFileLBS "test/data/cs.csv"
        CS.csvToLedger csv
          `shouldBe` Right
            ( LedgerReport
                [ [transactionQQ|
                     2022/09/29 * GOOG Sale
                       Assets:Investments:Charles Schwab:GOOG    GOOG -40 @ USD 98.5606
                       Assets:Liquid:Charles Schwab:USD                     USD 3942.33
                       Expenses:Financial Services                             USD 0.09|]
                , [transactionQQ|
                     2022/09/29 * Withholding Tax
                         Assets:Liquid:Charles Schwab:USD                        USD 1538.33
                         Equity:Charles Schwab:Unvested GOOG Withholding Tax|]
                , [transactionQQ|
                     2022/10/28 * Credit Interest
                         Assets:Liquid:Charles Schwab:USD        USD 0.07
                         Income:Google|]
                ]
                []
            )
