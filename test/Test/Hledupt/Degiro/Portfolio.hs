module Test.Hledupt.Degiro.Portfolio (
  tests,
) where

import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (parseTransactionUnsafe)
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Degiro.Portfolio (csvStatementToLedger)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Degiro.Portfolio" $ do
    describe "csvRecordsToLedger" csvRecordsToLedgerTests

csvRecordsToLedgerTests :: SpecWith ()
csvRecordsToLedgerTests = do
  it "Parses the csv file" $ do
    let csv =
          "Product,Symbol/ISIN,Amount,Closing,Local value,Value in CHF\n\
          \CASH & CASH FUND & FTX CASH (CHF),,,,CHF 143.20,143.20\n\
          \ISHARES E GOV1-3,IE00B14X4Q57,10,144.01,EUR 1440.10,100\n\
          \ISHARES MSCI WOR A,IE00B4L5Y983,10,59.44,EUR 594,40,700"
    csvStatementToLedger (fromGregorian 2010 01 01) csv
      `shouldBe` Right
        ( LedgerReport
            [ parseTransactionUnsafe
                "2010/01/01 * Degiro Status\n\
                \  Assets:Liquid:Degiro  CHF 0 = CHF 143.20\n\
                \  Assets:Investments:Degiro:IBGS  0 IBGS = IBGS 10\n\
                \  Assets:Investments:Degiro:IWDA  0 IWDA = IWDA 10"
            ]
            [ MarketPrice
                (fromGregorian 2010 01 01)
                "IBGS"
                "EUR"
                144.01
            , MarketPrice
                (fromGregorian 2010 01 01)
                "IWDA"
                "EUR"
                59.44
            ]
        )
