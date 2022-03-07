module Test.Hledupt.Data.LedgerReport (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
 )
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Data.LedgerReport" $ do
    describe "showLedgerReport" $ do
      it "Formats LedgerReport" $ do
        showLedgerReport
          ( LedgerReport
              [ parseTransactionUnsafe
                  "2020/11/26 IB Status\n\
                  \  Assets:Investments:IB:ACWF  0 ACWF = ACWF 123\n\
                  \  Assets:Liquid:IB:CHF  CHF 0 = CHF 100.0011305"
              , parseTransactionUnsafe
                  "2020/01/20 IB Deposit/Withdrawal\n\
                  \*  Assets:Liquid:IB:CHF  CHF 100.32\n\
                  \!  Todo"
              ]
              [ MarketPrice
                  (fromGregorian 2020 11 26)
                  "ACWF"
                  "USD"
                  (fromRational $ 3224 % 100)
              ]
          )
          `shouldBe` "2020-01-20 IB Deposit/Withdrawal\n\
                     \    * Assets:Liquid:IB:CHF    CHF 100.32\n\
                     \    ! Todo\n\
                     \\n\
                     \2020-11-26 IB Status\n\
                     \    Assets:Investments:IB:ACWF               0 = ACWF 123\n\
                     \    Assets:Liquid:IB:CHF                     0 = CHF 100.00\n\
                     \\n\
                     \P 2020-11-26 ACWF 32.24 USD\n"
