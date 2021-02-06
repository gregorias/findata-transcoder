{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Data.MyDecimal (tests) where

import qualified Data.Csv as Csv
import Data.Decimal (realFracToDecimal)
import Data.Ratio ((%))
import Hledupt.Data.MyDecimal (MyDecimal (..), decimalP, fromUnitsAndCents)
import Relude
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Text.Megaparsec (parseMaybe)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Data.MyDecimal" $ do
    describe "fromUnitsAndCents" $ do
      it "correctly handles negative numbers" $ do
        fromUnitsAndCents (-5) 50 `shouldBe` realFracToDecimal 2 (-5.50 :: Double)

    describe "decimalP" $ do
      it "parses a negative number" $ do
        parseMaybe decimalP "-123.321"
          `shouldBe` Just (fromRational $ -123 - (321 % 1000))
      it "parses a positive number" $ do
        parseMaybe decimalP "123.321"
          `shouldBe` Just (fromRational $ 123 + (321 % 1000))
      it "parses a unit" $ do
        parseMaybe decimalP "1"
          `shouldBe` Just (fromRational 1)
      it "parses a fraction" $ do
        parseMaybe decimalP "0.01"
          `shouldBe` Just (fromRational $ 1 % 100)

    describe "Csv.FromField MyDecimal" $ do
      it "Parses a decimal" $ do
        Csv.runParser (Csv.parseField "123.321") `shouldBe` Right (MyDecimal (fromRational $ 123 + (321 % 1000)))
