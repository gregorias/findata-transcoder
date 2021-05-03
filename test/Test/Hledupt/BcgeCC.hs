module Test.Hledupt.BcgeCC (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import qualified Hledupt.BcgeCC as BcgeCC
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Finpension" $ do
    describe "rechnungToLedger" $ do
      it "convertBCGECCRechnung" convertsBcgeCCRechnungTest

convertsBcgeCCRechnungTest :: IO ()
convertsBcgeCCRechnungTest = do
  rechnung <- readFileText "test/data/bcgecc.txt"
  let expectedTrs =
        [ parseTransactionUnsafe
            [trimming|
            2021/03/01 * LASTPASS.COM
              Assets:Liquid:BCGE CC  -34.20 CHF
              Expenses:Financial Services  0.60 CHF
              Expenses:Other  33.60 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/03/05 * PAYPAL *GITHUB INC
              Assets:Liquid:BCGE CC  -2.60 CHF
              Expenses:Financial Services  0.05 CHF
              Expenses:Other  2.55 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/03/06 * PAYPAL *GITHUB INC
              Assets:Liquid:BCGE CC  -5.90 CHF
              Expenses:Financial Services  0.10 CHF
              Expenses:Other  5.80 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/03/17 * GOOGLE *SERVICES, g.co/helppay#
              Assets:Liquid:BCGE CC  -10.50 CHF
              Expenses:Financial Services  0.20 CHF
              Expenses:Other  10.30 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/03/27 * SBB EasyRide, Bern 65 SBB
              Assets:Liquid:BCGE CC  -3.10 CHF
              Expenses:Other  3.10 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/04/16 * PAYPAL *STEAM GAMES
              Assets:Liquid:BCGE CC  -48.85 CHF
              Expenses:Financial Services  0.85 CHF
              Expenses:Other  48.00 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/04/19 * Power Food AG, Schaffhausen
              Assets:Liquid:BCGE CC  -83.70 CHF
              Expenses:Other  83.70 CHF|]
        , parseTransactionUnsafe
            [trimming|
            2021/04/23 * BCGE CC Status
              Assets:Liquid:BCGE CC  0.0 CHF = 292.70 CHF|]
        ]
  BcgeCC.rechnungToLedger rechnung
    `shouldBe` Right expectedTrs
