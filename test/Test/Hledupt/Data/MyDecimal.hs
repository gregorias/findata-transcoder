{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Hledupt.Data.MyDecimal (tests) where

import qualified Data.Csv as Csv
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Ratio ((%))
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (OptionalUnlimitedDecimalFraction, TwoDigitDecimalFraction),
  MyDecimal (..),
  decimalP,
  defaultDecimalFormat,
  fromUnitsAndCents,
 )
import Relude
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string)

tests :: SpecWith ()
tests = do
  describe "Hledupt.Data.MyDecimal" $ do
    describe "fromUnitsAndCents" $ do
      it "correctly handles negative numbers" $ do
        fromUnitsAndCents (-5) 50 `shouldBe` realFracToDecimal 2 (-5.50 :: Double)

    describe "default decimalP" $ do
      let (defaultDecimalP :: Parsec () Text Decimal) = decimalP defaultDecimalFormat
      it "parses a negative number" $ do
        parseMaybe defaultDecimalP "-123.321"
          `shouldBe` Just (fromRational $ -123 - (321 % 1000))
      it "parses a positive number" $ do
        parseMaybe defaultDecimalP "123.321"
          `shouldBe` Just (fromRational $ 123 + (321 % 1000))
      it "parses a unit" $ do
        parseMaybe defaultDecimalP "1"
          `shouldBe` Just (fromRational 1)
      it "parses a fraction" $ do
        parseMaybe defaultDecimalP "0.01"
          `shouldBe` Just (fromRational $ 1 % 100)

    describe "various decimalP" $ do
      it "parses a number with commas" $ do
        parseMaybe (decimalP (DecimalFormat (ChunkSep ',') Nothing)) "2,000,000"
          `shouldBe` Just (fromRational 2000000)

      it "parses a number with commas" $ do
        parseMaybe (decimalP (DecimalFormat (ChunkSep ',') (Just OptionalUnlimitedDecimalFraction))) "2,000,000"
          `shouldBe` Just (fromRational 2000000)

      it "parses only two digits when TwoDigit format is set" $ do
        parseMaybe
          ( decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction))
              <* string "123"
          )
          "2.00123"
          `shouldBe` Just (fromRational 2)

    describe "Csv.FromField MyDecimal" $ do
      it "Parses a decimal" $ do
        Csv.runParser (Csv.parseField "123.321")
          `shouldBe` Right (MyDecimal (fromRational $ 123 + (321 % 1000)))
      it "Parses a whole decimal with commas" $ do
        Csv.runParser (Csv.parseField "2,000,000")
          `shouldBe` Right (MyDecimal (fromRational 2000000))
