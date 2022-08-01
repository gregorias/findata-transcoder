module Test.Transcoder.Finpension.PortfolioTotals (tests) where

import Data.Time (fromGregorian)
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.Finpension.PortfolioTotals as Finpension

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Finpension.PortfolioTotals" $ do
    it "convertsTransactions" convertsTotalsText

convertsTotalsText :: IO ()
convertsTotalsText = do
  let totals =
        "Portfolio 1 6'824.94 CHF\n\
        \Portfolio 2 6'121.48 CHF\n"
      today = fromGregorian 2010 01 01
      expectedStatusTransaction =
        [transactionQQ|
           2010/01/01 * Finpension Status
             Assets:Investments:Finpension  0 CHF = CHF 12946.42|]
  Finpension.portfolioTotalsToStatusTransaction today totals `shouldBe` Right expectedStatusTransaction
