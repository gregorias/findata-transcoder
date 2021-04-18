{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Coop (
  tests,
) where

import Hledger.Read.TestUtils (parseTransactionUnsafe)
import qualified Hledupt.Coop as Coop
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
                "2021/04/09 * Coop\n\
                \  ! Assets:Liquid:BCGE  -6.10 CHF\n\
                \  Expenses:Groceries:Chewing Gum  1.60 CHF\n\
                \  Expenses:Groceries  4.50 CHF"
        Coop.receiptToLedger coop `shouldBe` Right expectedTr
