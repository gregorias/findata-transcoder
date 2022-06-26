module Test.Transcoder.Data.LedgerReport (
  tests,
) where

import Data.Ratio ((%))
import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.LedgerReport (
  LedgerReport (..),
  showLedgerReport,
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.Data.LedgerReport" $ do
    describe "showLedgerReport" $ do
      it "Formats LedgerReport" $ do
        showLedgerReport
          ( LedgerReport
              [ [transactionQQ|
                  2020/11/26 IB Status
                    Assets:Investments:IB:ACWF  0 ACWF = ACWF 123
                    Assets:Liquid:IB:CHF  CHF 0 = CHF 100.0011305|]
              , [transactionQQ|
                  2020/01/20 IB Deposit/Withdrawal
                  *  Assets:Liquid:IB:CHF  CHF 100.32
                  !  Todo|]
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
      it "Neatly formats a single transaction w/o extra lines" $ do
        showLedgerReport
          ( LedgerReport
              [ [transactionQQ|
                  2022/06/26 Foo
                    Assets:A  100 PLN
                    Assets:B  -100 PLN|]
              ]
              []
          )
          `shouldBe` "2022-06-26 Foo\n\
                     \    Assets:A      PLN 100.00\n\
                     \    Assets:B     PLN -100.00\n"
