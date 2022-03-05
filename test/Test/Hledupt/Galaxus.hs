module Test.Hledupt.Galaxus (
  tests,
) where

import Hledger.Read.TestUtils (transactionQQ)
import qualified Hledupt.Galaxus as Galaxus
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Galaxus" $ do
    describe "parseReceipt" $ do
      it "converts to a transaction" $ do
        receipt <- readFileText "test/data/galaxus0.txt"
        let expectedTr =
              [transactionQQ|
                2022/02/09 * Digitec/Galaxus
                  ! Assets:Liquid:BCGE  -769.40 CHF
                  ! Todo                 729.00 CHF ; Samsung Tab S8 (11 ", 128 GB, Graphite)
                  ! Todo                  31.40 CHF ; Onyx Eingabestift Boox Pen Plus Blau
                  ! Todo                   9.00 CHF ; Mindermengenzuschlag
                  |]
        Galaxus.parseReceipt receipt `shouldBe` Right expectedTr
