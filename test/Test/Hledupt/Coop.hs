module Test.Hledupt.Coop (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import qualified Hledupt.Coop as Coop
import qualified Hledupt.Coop.Config as Coop
import NeatInterpolation (trimming)
import Relude
import Test.HUnit (assertFailure)
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)

readCoopConfig :: Text -> IO Coop.Config
readCoopConfig json =
  either
    (\e -> assertFailure $ "Could not decode the config. " <> toString e)
    return
    (Coop.decodeConfig $ encodeUtf8 json)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledupt.Coop" $ do
    describe "receiptToLedger Coop.emptyConfig" $ do
      it "convertsToATransaction" $ do
        coop <- readFileText "test/data/coop.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE  -6.10 CHF
                    Expenses:Groceries  4.50 CHF
                    Expenses:Groceries:Chewing Gum  1.60 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyAssignsCategories" $ do
        coop <- readFileText "test/data/coop-cat.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -0.17 CHF
                  Expenses:Haushalt               0.11 CHF
                  Expenses:Gesundheit             0.02 CHF
                  Expenses:Groceries:Chewing Gum  0.01 CHF
                  Expenses:Groceries:Ready Meals  0.03 CHF|]

        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyHandlesARabatt" $ do
        coop <- readFileText "test/data/coop-rabatt.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE  -44.35 CHF
                  Expenses:Groceries     37.35 CHF
                  Expenses:Haushalt       9.40 CHF
                  Expenses:Other         -2.40 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyCountsAUnnotatedRabatt" $ do
        coop <- readFileText "test/data/coop-spargel-rabatt.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                2021/04/09 * Coop
                  ! Assets:Liquid:BCGE           -9.90 CHF
                  Expenses:Groceries             11.90 CHF
                  Expenses:Other                 -2.00 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctlyRecognizesMyCC" $ do
        coop <- readFileText "test/data/coop-card.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC        -14.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly handles multiple payment methods with supercash" $ do
        coop <- readFileText "test/data/coop-supercash.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    Expenses:Other                -7.00 CHF
                    ! Assets:Liquid:BCGE CC         -7.95 CHF
                    Expenses:Groceries:Ready Meals  14.95 CHF|]
        Coop.receiptToLedger Coop.emptyConfig coop `shouldBe` Right expectedTr

      it "correctly applies debtor postings" $ do
        config <-
          readCoopConfig
            [trimming|
                    {
                      "shared": [
                        {"product": "Stimorol",   "debtors": ["John Doe"]}
                      ]
                    }
                |]
        coop <- readFileText "test/data/coop-debt.txt"
        let expectedTr =
              parseTransactionUnsafe
                [trimming|
                  2021/04/09 * Coop
                    ! Assets:Liquid:BCGE CC  -4.40 CHF
                    Expenses:Groceries  2.90 CHF
                    Expenses:Groceries:Chewing Gum  0.75 CHF
                    ! Assets:Debts:John Doe           0.75 CHF|]
        Coop.receiptToLedger config coop `shouldBe` Right expectedTr
