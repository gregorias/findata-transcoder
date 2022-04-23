module Test.Hledger.Data.Extra (tests) where

import Hledger.Data.Amount (num, showAmount)
import qualified Hledger.Data.Extra as HDE
import Relude
import Test.Hspec (describe, it)
import qualified Test.Hspec as Hspec
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (fromUnitsAndCents)

tests :: Hspec.SpecWith ()
tests = do
  describe "Hledger.Data.Extra" $ do
    describe "makeCurrencyAmount" $ do
      it "Creates an amount with correct currency style" $
        do
          showAmount (HDE.makeCurrencyAmount chf (fromUnitsAndCents 3 50))
          `Hspec.shouldBe` "CHF 3.50"
    describe "setCurrencyPrecision" $ do
      it "Sets a 2 digit precision" $
        do
          showAmount (num 1.9590 & HDE.setCurrencyPrecision)
          `Hspec.shouldBe` "1.96"
