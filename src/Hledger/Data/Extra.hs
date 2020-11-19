module Hledger.Data.Extra (
  makeCurrencyAmount
) where

import           Control.Lens        (over, set, (&))
import           Data.String         (IsString (..))
import           Hledger.Data.Amount (num)
import           Hledger.Data.Types  (Amount (..), CommoditySymbol (..),
                                      Quantity (..))

import           Data.Text           (pack)
import           Hledger.Data.Lens   (aCommodity, aStyle, asCommoditySpaced,
                                      asPrecision)

makeCurrencyAmount :: String -> Quantity -> Amount
makeCurrencyAmount currency quantity =
  num quantity
  & set aCommodity (pack currency)
  . over aStyle (
    set asPrecision 2
    . set asCommoditySpaced True)
