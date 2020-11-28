module Test.Data where

import Data (fromUnitsAndCents)
import Data.Decimal (realFracToDecimal)
import Test.Hspec (SpecWith, describe, it, shouldBe)

dataTests :: SpecWith ()
dataTests = do
  describe "Data tests" $ do
    describe "fromUnitsAndCents" $ do
      it "correctly handles negative numbers" $ do
        fromUnitsAndCents (-5) 50 `shouldBe` realFracToDecimal 2 (-5.50 :: Double)
