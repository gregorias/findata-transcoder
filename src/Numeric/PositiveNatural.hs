-- Provides a data type representing positive natural numbers.
module Numeric.PositiveNatural (
  PositiveNatural,
  toNatural,
  fromNatural,
  positiveNaturalP,
  one,
) where

import Relude hiding (one)
import Text.Megaparsec (MonadParsec, Token)
import Text.Megaparsec.Char.Lexer (decimal)

newtype PositiveNatural = PositiveNatural {toNatural :: Natural}
  deriving stock (Eq, Ord)
  deriving newtype (Show)

fromNatural :: Natural -> Maybe PositiveNatural
fromNatural 0 = Nothing
fromNatural n = Just $ PositiveNatural n

positiveNaturalP :: (MonadFail m, Token s ~ Char, MonadParsec e s m) => m PositiveNatural
positiveNaturalP = do
  d :: Integer <- decimal
  when (d <= 0) $ fail ("Expected a positive natural number but got '" <> show d <> "'.\n")
  return . PositiveNatural . fromInteger $ d

one :: PositiveNatural
one = PositiveNatural 1
