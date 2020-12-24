{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}

module Hledger.Data.Extra
  ( makeCurrencyAmount,
    makeCommodityAmount,
    setCurrencyPrecision,
  )
where

import Control.Lens (over, set)
import Data.Text (pack)
import Hledger.Data.Amount (num)
import Hledger.Data.Lens
  ( aCommodity,
    aStyle,
    asCommoditySpaced,
    asPrecision,
  )
import Hledger.Data.Types
  ( Amount (..),
    AmountPrecision (..),
    Quantity,
  )
import Hledupt.Data.Currency (Currency)
import Relude

makeCommodityAmount :: String -> Quantity -> Amount
makeCommodityAmount commodity quantity =
  num quantity
    & set aCommodity (pack commodity)
      . set (aStyle . asCommoditySpaced) True

setCurrencyPrecision :: Amount -> Amount
setCurrencyPrecision =
  over
    aStyle
    ( set asPrecision (Precision 2)
        . set asCommoditySpaced True
    )

makeCurrencyAmount :: Currency -> Quantity -> Amount
makeCurrencyAmount currency quantity =
  makeCommodityAmount (show currency) quantity
    & setCurrencyPrecision
