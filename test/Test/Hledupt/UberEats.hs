module Test.Hledupt.UberEats (
  tests,
) where

import Hledger.Read.TestUtils (transactionQQ)
import qualified Hledupt.UberEats as UberEats
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.UberEats" $ do
    it "Parses a serialized bill" $ do
      let bill = "Payments Mastercard ••••12342/22/22 8:44 PMCHF 34.00"
      let expectedTr =
            [transactionQQ|
              2022/02/22 * Uber Eats
                ! Assets:Liquid:BCGE CC  -34 CHF
                Expenses:Take Away        34 CHF|]
      UberEats.parseBill bill `shouldBe` Right expectedTr
