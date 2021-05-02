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
        parseTransactionUnsafe
          [trimming|
            2021/04/23 * BCGE CC Status
              Assets:Liquid:BCGE CC  0.0 CHF = 292.70 CHF|]
  BcgeCC.rechnungToLedger rechnung
    `shouldBe` Right expectedTrs
