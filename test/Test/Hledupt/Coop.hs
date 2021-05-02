module Test.Hledupt.Coop (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import qualified Hledupt.Coop as Coop
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Coop" $ do
    describe "receiptToLedger" $ do
      it "convertsToATransaction" $ do
        coop <- readFileText "test/data/coop.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE  -6.10 CHF
                    Expenses:Groceries:Chewing Gum  1.60 CHF
                    Expenses:Groceries  4.50 CHF|]
        Coop.receiptToLedger coop `shouldBe` Right expectedTr

      it "correctlyAssignsCategories" $ do
        coop <- readFileText "test/data/coop-cat.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -0.12 CHF
                  Expenses:Gesundheit             0.02 CHF
                  Expenses:Haushalt               0.06 CHF
                  Expenses:Groceries:Chewing Gum  0.01 CHF
                  Expenses:Groceries:Ready Meals  0.03 CHF|]

        Coop.receiptToLedger coop `shouldBe` Right expectedTr

      it "correctlyHandlesARabatt" $ do
        coop <- readFileText "test/data/coop-rabatt.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -44.35 CHF
                  Expenses:Haushalt       9.40 CHF
                  Expenses:Groceries     37.35 CHF
                  Expenses:Other         -2.40 CHF|]
        Coop.receiptToLedger coop `shouldBe` Right expectedTr

      it "correctlyRecognizesMyCC" $ do
        coop <- readFileText "test/data/coop-card.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC        -14.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF|]
        Coop.receiptToLedger coop `shouldBe` Right expectedTr
