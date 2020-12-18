{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hledupt.Data (dataTests) where

import qualified Data.Csv as Csv
import Data.Decimal (realFracToDecimal)
import Data.Ratio ((%))
import Hledupt.Data (MyDecimal (..), decimalParser, fromUnitsAndCents)
import Relude
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Text.Megaparsec (parseMaybe)

dataTests :: SpecWith ()
dataTests = do
  describe "Hledupt.Data" $ do
    describe "fromUnitsAndCents" $ do
      it "correctly handles negative numbers" $ do
        fromUnitsAndCents (-5) 50 `shouldBe` realFracToDecimal 2 (-5.50 :: Double)

    describe "decimalParser" $ do
      it "parses a negative number" $ do
        parseMaybe decimalParser "-123.321"
          `shouldBe` Just (fromRational $ -123 - (321 % 1000))
      it "parses a positive number" $ do
        parseMaybe decimalParser "123.321"
          `shouldBe` Just (fromRational $ 123 + (321 % 1000))
      it "parses a unit" $ do
        parseMaybe decimalParser "1"
          `shouldBe` Just (fromRational 1)
      it "parses a fraction" $ do
        parseMaybe decimalParser "0.01"
          `shouldBe` Just (fromRational $ 1 % 100)

    describe "Csv.FromField MyDecimal" $ do
      it "Parses a decimal" $ do
        Csv.runParser (Csv.parseField "123.321") `shouldBe` Right (MyDecimal (fromRational $ 123 + (321 % 1000)))
