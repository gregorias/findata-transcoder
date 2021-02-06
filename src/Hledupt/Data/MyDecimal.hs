{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}

-- | My extended parsing utilities for parsing decimals.
module Hledupt.Data.MyDecimal (
  MyDecimal (..),
  fromUnitsAndCents,
  myDecDec,
  decimalP,
) where

import qualified Control.Lens.Iso as Lens
import Data.ByteString.Char8 (unpack)
import Data.Char (digitToInt)
import qualified Data.Csv as Csv
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Ratio ((%))
import Relude
import Text.Megaparsec (MonadParsec, Token, Tokens, anySingle)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Stream (tokensToChunk)

-- | A Decimal wrapper that is extended with parsing functions.
newtype MyDecimal = MyDecimal Decimal
  deriving stock (Eq, Show)

decimalFractionP :: (MonadParsec e s m, Token s ~ Char) => m Rational
decimalFractionP = do
  fractionalString <- MP.some digitChar
  return $
    uncurry (%)
      . foldl' (\(v, b) d -> (v + d * b, b * 10)) (0, 1)
      . dropWhile (== 0)
      . fmap (toInteger . digitToInt)
      . reverse
      $ fractionalString

cleanUpCommas :: (MonadFail m, MonadParsec e s m, Token s ~ Char, Tokens s ~ s) => Proxy s -> m ()
cleanUpCommas proxy = do
  tokens <- many anySingle
  let tokensWoComma :: [Char] = filter (/= ',') tokens
  MP.setInput . tokensToChunk proxy $ tokensWoComma

-- | A decimal parser
--
-- >>> parseMaybe decimalP "-2,000.41"
-- 2000.41
decimalP :: (MonadFail m, MonadParsec e s m, Token s ~ Char, Tokens s ~ s) => m Decimal
decimalP = signed space $ do
  cleanUpCommas Proxy
  units :: Integer <- decimal
  fract <- (char '.' >> decimalFractionP) <|> pure 0
  return $ fromRational (units % 1 + fract)

fromUnitsAndCents :: Integer -> Integer -> Decimal
fromUnitsAndCents units cents = unitsDec `op` centsDec
 where
  unitsDec = realFracToDecimal 0 (units % 1)
  centsDec = realFracToDecimal 2 (cents % 100)
  op = if units >= 0 then (+) else (-)

myDecDec :: Lens.Iso' MyDecimal Decimal
myDecDec = Lens.iso (\(MyDecimal d) -> d) MyDecimal

instance Csv.FromField MyDecimal where
  parseField field =
    maybe
      (fail "Could not parse a decimal")
      (pure . MyDecimal)
      $ MP.parseMaybe decimalP (unpack field)
