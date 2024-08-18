module Test.Hledger.Data.Extra (tests) where

import Data.Decimal.Extra (fromUnitsAndCents)
import Hledger.Data.Amount (num, showAmount)
import Hledger.Data.Extra qualified as HDE
import Relude
import Test.Hspec (describe, it)
import Test.Hspec qualified as Hspec
import Test.Hspec.Expectations.Pretty (shouldBe)
import Transcoder.Data.Currency (chf)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledger.Data.Extra" $ do
    describe "makeCommodityAmount" $ do
      it "takes into account the precision of the quantity"
        $ showAmount (HDE.makeCommodityAmount "GOOG" 40.045)
        `shouldBe` "GOOG 40.045"

    describe "makeCurrencyAmount" $ do
      it "Creates an amount with correct currency style"
        $ showAmount (HDE.makeCurrencyAmount chf (fromUnitsAndCents 3 50))
        `shouldBe` "CHF 3.50"
    describe "setCurrencyPrecision" $ do
      it "Sets a 2 digit precision"
        $ showAmount (num 1.9590 & HDE.setCurrencyPrecision)
        `shouldBe` "1.96"
