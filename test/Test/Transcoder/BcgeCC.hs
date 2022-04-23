module Test.Transcoder.BcgeCC (
  tests,
) where

import Hledger.Read.TestUtils (transactionQQ)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import qualified Transcoder.BcgeCC as BcgeCC

tests :: Hspec.SpecWith ()
tests = do
  describe "Transcoder.Finpension" $ do
    describe "rechnungToLedger" $ do
      it "convertBCGECCRechnung-01" convertsBcgeCCRechnungTest01
      it "convertBCGECCRechnung-02" convertsBcgeCCRechnungTest02

convertsBcgeCCRechnungTest01 :: IO ()
convertsBcgeCCRechnungTest01 = do
  rechnung <- readFileText "test/data/bcgecc-01.txt"
  let expectedTrs =
        [ [transactionQQ|
          2021/03/01 * LASTPASS.COM
            Assets:Liquid:BCGE CC  -34.20 CHF
            Expenses:Financial Services  0.60 CHF
            Expenses:Other  33.60 CHF|]
        , [transactionQQ|
          2021/03/05 * PAYPAL *GITHUB INC
            Assets:Liquid:BCGE CC  -2.60 CHF
            Expenses:Financial Services  0.05 CHF
            Expenses:Other  2.55 CHF|]
        , [transactionQQ|
          2021/03/06 * PAYPAL *GITHUB INC
            Assets:Liquid:BCGE CC  -5.90 CHF
            Expenses:Financial Services  0.10 CHF
            Expenses:Other  5.80 CHF|]
        , [transactionQQ|
          2021/03/17 * GOOGLE *SERVICES, g.co/helppay#
            Assets:Liquid:BCGE CC  10.50 CHF
            Expenses:Financial Services  -0.20 CHF
            Expenses:Other  -10.30 CHF|]
        , [transactionQQ|
          2021/03/27 * SBB EasyRide, Bern 65 SBB
            Assets:Liquid:BCGE CC  -3.10 CHF
            Expenses:Other  3.10 CHF|]
        , [transactionQQ|
          2021/04/16 * PAYPAL *STEAM GAMES
            Assets:Liquid:BCGE CC  -48.85 CHF
            Expenses:Financial Services  0.85 CHF
            Expenses:Other  48.00 CHF|]
        , [transactionQQ|
          2021/04/19 * Power Food AG, Schaffhausen
            Assets:Liquid:BCGE CC  -83.70 CHF
            Expenses:Other  83.70 CHF|]
        , [transactionQQ|
          2021/04/23 * BCGE CC Status
            Assets:Liquid:BCGE CC  0.0 CHF = -292.70 CHF|]
        ]
  BcgeCC.rechnungToLedger rechnung
    `shouldBe` Right expectedTrs

convertsBcgeCCRechnungTest02 :: IO ()
convertsBcgeCCRechnungTest02 = do
  rechnung <- readFileText "test/data/bcgecc-02.txt"
  let expectedTrs =
        [ [transactionQQ|
          2022/01/23 * UBER *EATS, HELP.UBER.COM
            Assets:Liquid:BCGE CC  -312.05 CHF
            Expenses:Other  312.05 CHF|]
        , [transactionQQ|
          2022/01/28 * UBER EATS HELP.UBER.CO, help.uber.com
            Assets:Liquid:BCGE CC  -922.51 CHF
            Expenses:Other  922.51 CHF|]
        , [transactionQQ|
          2022/02/23 * BCGE CC Status
            Assets:Liquid:BCGE CC  0.0 CHF = -1234.56 CHF|]
        ]
  BcgeCC.rechnungToLedger rechnung
    `shouldBe` Right expectedTrs
