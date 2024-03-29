module Test.Transcoder.Degiro.Portfolio (
  tests,
) where

import Data.Time (fromGregorian)
import Hledger (MarketPrice (MarketPrice))
import Hledger.Read.TestUtils (transactionsQQ)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))
import Transcoder.Degiro.Portfolio (csvStatementToLedger)

tests :: SpecWith ()
tests = do
  describe "Transcoder.Degiro.Portfolio" $ do
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
            [transactionsQQ|
              2010/01/01 * Degiro Status
                Assets:Liquid:Degiro  CHF 0 = CHF 143.20
                Assets:Investments:Degiro:IBGS  0 IBGS = IBGS 10
                Assets:Investments:Degiro:IWDA  0 IWDA = IWDA 10|]
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
