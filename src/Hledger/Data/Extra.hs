module Hledger.Data.Extra (
  ToPosting (..),
  ToTransaction (..),
  makeCashAmount,
  makeCurrencyAmount,
  makeCommodityAmount,
  makeTransaction,
  setCurrencyPrecision,
) where

import Control.Lens (over, set)
import Data.Time (Day)
import Hledger (Posting, Status, Transaction, transaction)
import Hledger.Data.Amount (num)
import Hledger.Data.Lens (
  aCommodity,
  aStyle,
  asCommoditySpaced,
  asPrecision,
  tDescription,
  tStatus,
 )
import Hledger.Data.Types (
  Amount (..),
  AmountPrecision (..),
  Quantity,
 )
import Hledupt.Data.Cash (Cash (Cash))
import Hledupt.Data.Currency (Currency)
import Relude

makeCommodityAmount :: Text -> Quantity -> Amount
makeCommodityAmount commodity quantity =
  num quantity
    & set aCommodity commodity
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

makeCashAmount :: Cash -> Amount
makeCashAmount (Cash currency quantity) = makeCurrencyAmount currency quantity

-- | Makes a Transaction.
--
-- This is a helper function that follows the order of definition in a normal transaction.
makeTransaction :: Day -> Maybe Status -> Text -> [Posting] -> Transaction
makeTransaction day maybeStatus description ps =
  transaction day ps
    & set tDescription description
      . maybe id (set tStatus) maybeStatus

class ToPosting a where
  toPosting :: a -> Posting

class ToTransaction a where
  toTransaction :: a -> Transaction
