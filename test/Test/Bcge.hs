module Test.Bcge where

import           Bcge               (BcgeTransaction (..),
                                     csvLineToBcgeTransaction)
import           Data               (fromUnitsAndCents)
import           Data.Time.Calendar (fromGregorian)
import           Test.Hspec         (describe, hspec, it, shouldBe)

bcgeTests = do
  describe "Bcge tests" $ do
    describe "csvLinesToBcgeTransaction" $ do
      it "parses a valid CSV line" $ do
        csvLineToBcgeTransaction ("06.11.20", "Title", "-5.50") `shouldBe`
          Just (BcgeTransaction (fromGregorian 2020 11 6) "Title"
            (fromUnitsAndCents (-5) 50))
