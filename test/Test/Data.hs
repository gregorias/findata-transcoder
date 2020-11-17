module Test.Data where

import           Data         (MonetaryValue, fromUnitsAndCents)
import           Data.Decimal (realFracToDecimal)
import           Test.Hspec   (describe, hspec, it, shouldBe)

dataTests = do
  describe "Data tests" $ do
    describe "fromUnitsAndCents" $ do
      it "correctly handles negative numbers" $ do
        fromUnitsAndCents (-5) 50 `shouldBe` realFracToDecimal 2 (-5.50)

