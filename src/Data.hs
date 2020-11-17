module Data(MonetaryValue(..), fromUnitsAndCents) where

import           Data.Decimal (Decimal, realFracToDecimal)
import           Data.Ratio   ((%))

-- | A type representing a monetary value, i.e., a decimal with 2 decimal
-- places
type MonetaryValue = Decimal


fromUnitsAndCents :: (Integral a) => a -> a -> MonetaryValue
fromUnitsAndCents units cents = unitsDec `op` centsDec
  where
    unitsDec = realFracToDecimal 0 (units % 1)
    centsDec = realFracToDecimal 2 (cents % 100)
    op = if units >= 0 then (+) else (-)
