module Hledger.Data.Extra (
  Comment (..),
  ToPosting (..),
  ToTransaction (..),
  makeCashAmount,
  makeCurrencyAmount,
  makeCommodityAmount,
  makePosting,
  makeTransaction,
  setCurrencyPrecision,
) where

import Control.Lens (over, set)
import Data.Cash (Cash (Cash))
import Data.Time (Day)
import Hledger (AccountName, Posting, Status, Transaction, missingamt, post, transaction)
import Hledger.Data.Amount (num)
import Hledger.Data.Lens (
  aCommodity,
  aStyle,
  asCommoditySpaced,
  asPrecision,
  pAmount,
  pComment,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledger.Data.Types (
  Amount (..),
  AmountPrecision (..),
  Quantity,
 )
import Relude
import Transcoder.Data.Currency (Currency)

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

data Comment
  = NoComment
  | Comment !Text

setComment :: Comment -> Posting -> Posting
setComment NoComment = set pComment ""
setComment (Comment comment) = set pComment comment

-- | Makes a Posting.
--
-- This is a helper function that follows the order of definition in a normal posting.
makePosting :: Maybe Status -> AccountName -> Maybe Cash -> Comment -> Posting
makePosting maybeStatus accountName maybeCash comment =
  post accountName missingamt
    & setComment comment
      . maybe id (set pStatus) maybeStatus
      . maybe id (set pAmount . makeCashAmount) maybeCash

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
