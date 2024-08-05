module Hledger.Data.Extra (
  Comment (..),
  makeCurrencyAmount,
  makeCommodityAmount,
  makePosting,
  makeTransaction,
  setCurrencyPrecision,

  -- * Conversion classes
  ToAmount (..),
  ToPosting (..),
  ToTransaction (..),
) where

import Control.Lens (over, set)
import Data.Maybe.Extra (ToMaybeX (..))
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

data Comment
  = NoComment
  | Comment !Text

setComment :: Comment -> Posting -> Posting
setComment NoComment = set pComment ""
setComment (Comment comment) = set pComment comment

-- | Makes a Posting.
--
-- This is a helper function that follows the order of definition in a normal posting.
makePosting ::
  (ToMaybeX Status toMaybeStatus, ToMaybeX Amount toMaybeAmount) =>
  toMaybeStatus -> AccountName -> toMaybeAmount -> Comment -> Posting
makePosting maybeStatus accountName maybeAmount comment =
  post accountName missingamt
    & setComment comment
    & maybe id (set pStatus) (toMaybeX maybeStatus)
    & maybe id (set pAmount) (toMaybeX maybeAmount)

-- | Makes a Transaction.
--
-- This is a helper function that follows the order of definition in a normal transaction.
makeTransaction ::
  (ToMaybeX Status maybeStatus) =>
  Day -> maybeStatus -> Text -> [Posting] -> Transaction
makeTransaction day maybeStatus description ps =
  transaction day ps
    & set tDescription description
    . maybe id (set tStatus) (toMaybeX maybeStatus)

class ToAmount a where
  toAmount :: a -> Amount

instance ToAmount Amount where
  toAmount = id

class ToPosting a where
  toPosting :: a -> Posting

class ToTransaction a where
  toTransaction :: a -> Transaction
