module Test.Hledger.Data.Extra (tests) where

import           Hledger.Data.Amount (showAmount)
import qualified Test.Hspec          as Hspec

import           Data                (fromUnitsAndCents)
import qualified Hledger.Data.Extra  as HDE

tests = do
  Hspec.describe "Hledger.Data.Extra tests" $ do
    Hspec.describe "makeCurrencyAmount" $ do
      Hspec.it "creates an amount with correct currency style" $ do
        showAmount (HDE.makeCurrencyAmount "CHF" (fromUnitsAndCents 3 50))
        `Hspec.shouldBe` "CHF 3.50"

