module Test.Transcoder.Revolut (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Relude
import Test.Hspec (describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Transcoder.Data.LedgerReport (LedgerReport (..))
import qualified Transcoder.Revolut as Revolut

tests :: Hspec.SpecWith ()
tests = do
  describe "Revolut" $ do
    it "parses a CSV report to a LedgerReport" $ do
      csv <- readFileLBS "test/data/revolut.csv"
      Revolut.parseCsvToLedger csv
        `shouldBe` Right
          ( LedgerReport
              [ parseTransactionUnsafe
                  "2021/08/11 * Top-Up by *7817\n\
                  \  Assets:Liquid:Revolut:CHF  CHF 15 = CHF 23.95\n\
                  \  ! Todo"
              , parseTransactionUnsafe
                  "2021/08/18 * Amzn Mktp De\n\
                  \  Assets:Liquid:Revolut:CHF  CHF -28.61 = CHF 104.43\n\
                  \  ! Todo"
              , parseTransactionUnsafe
                  "2021/08/20 * To John Doe\n\
                  \  Assets:Liquid:Revolut:CHF  CHF -11.62 = CHF 92.81\n\
                  \  ! Todo"
              , parseTransactionUnsafe
                  "2021/08/26 * Exchanged to PLN\n\
                  \  Assets:Liquid:Revolut:CHF  CHF -86.05 = CHF 0\n\
                  \  ! Todo"
              ]
              []
          )
