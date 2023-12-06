{-# LANGUAGE ExtendedDefaultRules #-}

module Test.Data.Decimal.Extra (tests) where

import Data.Aeson qualified as Aeson
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Decimal.Extra (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (OptionalUnlimitedDecimalFraction, TwoDigitDecimalFraction),
  decimalP,
  defaultDecimalFormat,
  fromUnitsAndCents,
  parseJSON,
 )
import Data.Ratio ((%))
import Data.Text qualified as T
import Relude
import Test.HUnit.Extra (assertLeft, textShouldContain)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string)

newtype DecimalWithCommas = DecimalWithCommas Decimal
  deriving newtype (Eq, Show)

instance Aeson.FromJSON DecimalWithCommas where
  parseJSON = fmap DecimalWithCommas . parseJSON (DecimalFormat (ChunkSep ',') Nothing)

tests :: SpecWith ()
tests = do
  describe "Data.Decimal.Extra" $ do
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

    describe "parseJSON" $ do
      it "parses a number with commas" $ do
        Aeson.decode @DecimalWithCommas "\"2,000,000\""
          `shouldBe` Just (DecimalWithCommas (fromRational 2000000))

      it "gives an understandable message when parsing fails" $ do
        let eitherDecimal = Aeson.eitherDecode @DecimalWithCommas "\"asdfasdf\""
        message <- assertLeft eitherDecimal
        T.pack message `textShouldContain` "Could not parse asdfasdf as a decimal."
