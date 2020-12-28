{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Data (
  fromUnitsAndCents,
  MyDecimal (..),
  myDecDec,
  decimalParser,
) where

import qualified Control.Lens.Iso as Lens
import Data.ByteString.Char8 (unpack)
import Data.Char (digitToInt)
import qualified Data.Csv as Csv
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Ratio ((%))
import Relude
import Text.Megaparsec (MonadParsec, Token)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

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

decimalParser :: (MonadFail m, MonadParsec e s m, Token s ~ Char) => m Decimal
decimalParser = signed space $ do
  units :: Integer <- decimal
  fract <- (char '.' >> decimalFractionParser) <|> pure 0
  return $ fromRational (units % 1 + fract)

fromUnitsAndCents :: Integer -> Integer -> Decimal
fromUnitsAndCents units cents = unitsDec `op` centsDec
 where
  unitsDec = realFracToDecimal 0 (units % 1)
  centsDec = realFracToDecimal 2 (cents % 100)
  op = if units >= 0 then (+) else (-)

newtype MyDecimal = MyDecimal Decimal
  deriving stock (Eq, Show)

myDecDec :: Lens.Iso' MyDecimal Decimal
myDecDec = Lens.iso (\(MyDecimal d) -> d) MyDecimal

instance Csv.FromField MyDecimal where
  parseField field =
    maybe
      (fail "Could not parse a decimal")
      (pure . MyDecimal)
      $ MP.parseMaybe decimalParser (unpack field)
