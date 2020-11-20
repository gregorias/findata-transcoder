module Test.Hledger.Data.Extra (tests) where

import Data (fromUnitsAndCents)
import Hledger.Data.Amount (showAmount)
import qualified Hledger.Data.Extra as HDE
import qualified Test.Hspec as Hspec

tests = do
  Hspec.describe "Hledger.Data.Extra tests" $ do
    Hspec.describe "makeCurrencyAmount" $ do
      Hspec.it "creates an amount with correct currency style" $
        do
          showAmount (HDE.makeCurrencyAmount "CHF" (fromUnitsAndCents 3 50))
          `Hspec.shouldBe` "CHF 3.50"
