module Test.Transcoder.Finpension.Transactions (
  tests,
) where

import qualified Control.Lens as L
import Hledger (amountSetFullPrecision)
import Hledger.Data.Lens (pAmounts, tPostings)
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))
import Transcoder.Finpension.Transactions (transactionsToLedger)

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Finpension.Transactions" $ do
    it "convertsTransactions" convertsTransactionsTest

convertsTransactionsTest :: IO ()
convertsTransactionsTest = do
  csv <- CsvFile <$> readFileLBS "test/data/finpension-transactions.csv"
  let expectedTrs =
        [transactionsQQ|
          2021/02/15 * Finpension Deposit
            Assets:Investments:Finpension:Cash  6883 CHF = 6883 CHF
            ! Todo
          2021/03/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  -137.74485 CHF = 6745.25515 CHF
            Assets:Investments:Finpension:CH Large Cap  0.097 "CH0033782431" @ 1420.05 CHF
          2021/03/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  -135.4793 CHF = 6609.77585 CHF
            Assets:Investments:Finpension:CH Small & Mid Cap  0.055 "CH0110869143" @ 2463.26 CHF
          2021/03/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  -1027.46762 CHF = 5582.30823 CHF
            Assets:Investments:Finpension:Emerging Markets  0.443 "CH0017844686" @ 2319.34 CHF
          2021/03/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  -700.01376 CHF = 4882.29447 CHF
            Assets:Investments:Finpension:World ex CH Small Cap  0.354 "CH0214967314" @ 1977.44 CHF
          2021/03/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  -4897.77028 CHF = -15.47581 CHF
            Assets:Investments:Finpension:World ex CH  2.503 "CH0130458182" @ 1956.76 CHF
          2021/04/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash           2.98974 CHF = -12.486070 CHF
            Assets:Investments:Finpension:CH Large Cap  -0.002   "CH0033782431" @ 1494.87 CHF
          2021/04/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash      CHF 137.26416 = CHF 124.77809
            Assets:Investments:Finpension:World ex CH    "CH0130458182" -0.066 @ CHF 2079.76
          2021/04/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash  4.153080 CHF = 128.931170 CHF
            Assets:Investments:Finpension:World ex CH Small Cap  -0.002 "CH0214967314" @ 2076.54 CHF
          2021/04/01 * Finpension Purchase/Sell
            Assets:Investments:Finpension:Cash     CHF -51.67206 = CHF 77.25911
            Assets:Investments:Finpension:Emerging Markets    "CH0017844686" 0.022 @ CHF 2348.73
          2021/04/07 * Finpension Fee
            Assets:Investments:Finpension:Cash          -4.9102 CHF = 72.348910 CHF
            Expenses:Financial Services                  4.9102 CHF
          2021/05/25 * Finpension Dividend -- CSIF (CH) Equity Emerging Markets Blue DB
            Assets:Investments:Finpension:Cash  6.027682 CHF = 78.376592 CHF
            Income:Capital Gains
          |]
          & L.over (L.each . tPostings . L.mapped . pAmounts) amountSetFullPrecision
  transactionsToLedger csv
    `shouldBe` Right
      ( LedgerReport expectedTrs []
      )
