module Test.Transcoder.Data.MyDecimal (tests) where

import Data.Csv qualified as Csv
import Data.Ratio ((%))
import Relude
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Transcoder.Data.MyDecimal (
  MyDecimal (..),
 )

tests :: SpecWith ()
tests = do
  describe "Transcoder.Data.MyDecimal" $ do
    describe "Csv.FromField MyDecimal" $ do
      it "Parses a decimal" $ do
        Csv.runParser (Csv.parseField "123.321")
          `shouldBe` Right (MyDecimal (fromRational $ 123 + (321 % 1000)))
      it "Parses a whole decimal with commas" $ do
        Csv.runParser (Csv.parseField "2,000,000")
          `shouldBe` Right (MyDecimal (fromRational 2000000))
