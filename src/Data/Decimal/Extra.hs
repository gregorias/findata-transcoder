{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Decimal.Extra (
  DecimalFormat (..),
  DecimalFractionFormat (..),
  ChunkSepFormat (..),
  chunkSepFormat,
  defaultDecimalFormat,
  cashDecimalFormat,

  -- * Parsing
  decimalP,
  parseJSON,

  -- * Utilities
  fromUnitsAndCents,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Char (digitToInt)
import Data.Decimal (Decimal, DecimalRaw (..), realFracToDecimal)
import Data.Ratio ((%))
import Data.Text qualified as T
import Language.Haskell.TH.Syntax (Lift)
import Relude
import Text.Megaparsec (MonadParsec, Token)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

deriving stock instance (Lift a) => Lift (DecimalRaw a)

-- | 'DecimalFractionFormat' describes the format of the fraction part of a
-- decimal.
data DecimalFractionFormat
  = TwoDigitDecimalFraction
  | OptionalUnlimitedDecimalFraction

data ChunkSepFormat = NoChunkSep | ChunkSep Char

chunkSepFormat :: a -> (Char -> a) -> ChunkSepFormat -> a
chunkSepFormat def _ NoChunkSep = def
chunkSepFormat _ f (ChunkSep c) = f c

-- | 'DecimalFormat' describes the string encoding of a decimal.
--
-- If a fraction part is present, then it starts with `.`
data DecimalFormat = DecimalFormat
  { decimalFormatChunkSep :: !ChunkSepFormat
  -- ^ The optional character used to separate 3 digit chunks.
  , decimalFormatFractionFormat :: !(Maybe DecimalFractionFormat)
  -- ^ 'decimalFormatFractionFormat' describes the format of an optional
  -- fraction part of a decimal.
  }

-- | The default format that has no chunk separation and can have a fractional
-- part of arbitrary length.
defaultDecimalFormat :: DecimalFormat
defaultDecimalFormat = DecimalFormat NoChunkSep (Just OptionalUnlimitedDecimalFraction)

-- | The format that has no chunk separation and has a two-digit
-- fractional part of arbitrary length.
cashDecimalFormat :: ChunkSepFormat -> DecimalFormat
cashDecimalFormat = flip DecimalFormat (Just TwoDigitDecimalFraction)

-- | A decimal parser that handles separators and fractions.
--
-- >>> MP.parseMaybe (decimalP defaultDecimalFormat) "-2000.41"
-- Just -2000.41
decimalP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  DecimalFormat ->
  m Decimal
decimalP (DecimalFormat chunkSep fractionFormat) = signed space $ do
  units :: Integer <- chunkSepFormat decimal decimalWithSepP chunkSep
  fract <- case fractionFormat of
    Nothing -> pure 0
    Just format -> decimalFractionP format
  return $ fromRational (units % 1 + fract)

decimalWithSepP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  Char ->
  m Integer
decimalWithSepP sep = do
  wholeString <- some (digitChar <|> char sep)
  let digits = fmap (toInteger . digitToInt) . filter (/= sep) $ wholeString
  return $ foldl' (\a d -> a * 10 + d) 0 digits

decimalFractionP ::
  ( MonadParsec e s m
  , Token s ~ Char
  ) =>
  DecimalFractionFormat ->
  m Rational
decimalFractionP TwoDigitDecimalFraction = do
  void $ char '.'
  fractionalString <- replicateM 2 digitChar
  return $ fractionStringToRational fractionalString
decimalFractionP OptionalUnlimitedDecimalFraction =
  ( do
      void $ char '.'
      fractionalString <- MP.some digitChar
      return $ fractionStringToRational fractionalString
  )
    <|> pure 0

fractionStringToRational :: [Char] -> Rational
fractionStringToRational =
  uncurry (%)
    . foldl' (\(v, b) d -> (v + d * b, b * 10)) (0, 1)
    . dropWhile (== 0)
    . fmap (toInteger . digitToInt)
    . reverse

fromUnitsAndCents :: Integer -> Integer -> Decimal
fromUnitsAndCents units cents = unitsDec `op` centsDec
 where
  unitsDec = realFracToDecimal 0 (units % 1)
  centsDec = realFracToDecimal 2 (cents % 100)
  op = if units >= 0 then (+) else (-)

-- | An Aeson-compatible parser for decimals with a format option.
--
-- Useful to define custom FromJSON instances.
parseJSON :: DecimalFormat -> Aeson.Value -> Aeson.Parser Decimal
parseJSON format = Aeson.withText "decimal" $ \text ->
  case MP.parseMaybe @Void (decimalP format) text of
    Nothing -> fail $ "Could not parse " <> T.unpack text <> " as a decimal."
    Just d -> return d
