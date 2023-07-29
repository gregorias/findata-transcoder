module Test.Transcoder.Revolut (
  tests,
) where

import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec qualified as Hspec
import Transcoder.Data.LedgerReport (LedgerReport (..))
import Transcoder.Revolut qualified as Revolut

tests :: Hspec.SpecWith ()
tests = do
  describe "Revolut" $ do
    it "parses a CSV report to a LedgerReport" $ do
      csv <- readFileLBS "test/data/revolut.csv"
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
