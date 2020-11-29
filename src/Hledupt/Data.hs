{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Data (MonetaryValue, fromUnitsAndCents, MyDecimal (..), myDecDec, decimalParser) where

import qualified Control.Lens.Iso as Lens
import Data.ByteString.Char8 (unpack)
import Data.Char (digitToInt)
import qualified Data.Csv as Csv
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Foldable (foldl')
import Data.Ratio ((%))
import Text.Megaparsec (MonadParsec, Token)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar)

decimalFractionParser :: (MonadParsec e s m, Token s ~ Char) => m Rational
decimalFractionParser = do
  fractionalString <- MP.some digitChar
  return $
    uncurry (%)
      . foldl' (\(v, b) d -> (v + d * b, b * 10)) (0, 1)
      . dropWhile (== 0)
      . fmap (toInteger . digitToInt)
      . reverse
      $ fractionalString

decimalParser :: (MonadParsec e s m, Token s ~ Char) => m Decimal
decimalParser = do
  negMod <- MP.try (char '-' >> pure negate) MP.<|> pure id
  unitsString <- MP.some digitChar
  units :: Integer <- return $ read unitsString
  fract <- (char '.' >> decimalFractionParser) MP.<|> pure 0
  return $ negMod $ fromRational (units % 1 + fract)

-- | A type representing a monetary value, i.e., a decimal with 2 decimal
-- places
type MonetaryValue = Decimal

fromUnitsAndCents :: Integer -> Integer -> MonetaryValue
fromUnitsAndCents units cents = unitsDec `op` centsDec
  where
    unitsDec = realFracToDecimal 0 (units % 1)
    centsDec = realFracToDecimal 2 (cents % 100)
    op = if units >= 0 then (+) else (-)

newtype MyDecimal = MyDecimal Decimal
  deriving (Eq, Show)

myDecDec :: Lens.Iso' MyDecimal Decimal
myDecDec = Lens.iso (\(MyDecimal d) -> d) MyDecimal

instance Csv.FromField MyDecimal where
  parseField field =
    maybe
      (fail "Could not parse a decimal")
      (pure . MyDecimal)
      $ MP.parseMaybe decimalParser (unpack field)
