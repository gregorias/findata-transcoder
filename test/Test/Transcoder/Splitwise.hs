module Test.Transcoder.Splitwise (
  tests,
) where

import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (transactionQQ)
import NeatInterpolation (trimming)
import Relude
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import qualified Transcoder.Splitwise as Splitwise

tests :: Hspec.SpecWith ()
tests = do
  Hspec.describe "Transcoder.Splitwise" $ do
    Hspec.it "Parses a statement" $ do
      let csv =
            CsvFile . encodeUtf8 $
              [trimming|
                    fname,lname,amount,currency
                    John,Doe,2.47,CHF
                |]
      Splitwise.statementToLedger (fromGregorian 2022 1 1) csv
        `shouldBe` Right
          [transactionQQ|
                 2022/01/01 * Splitwise Balance
                   Assets:Debts:John Doe  = CHF 2.47
                |]
