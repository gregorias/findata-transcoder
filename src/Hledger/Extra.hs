{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hledger.Extra (
  showTransaction,
) where

import Data.Decimal.Extra ()
import Data.Text qualified as T
import Data.Time.Extra ()
import Hledger (Transaction)
import Hledger qualified as H
import Hledger.Data.Types qualified as H
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift.Generics (genericLiftTypedCompat)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude
import Text.Megaparsec.Extra ()

-- | Formats a Transaction into Ledger format.
--
-- Unlike 'showTransaction' from Hledger, this function avoids unnecessary newlines.
showTransaction :: Transaction -> Text
showTransaction = (<> "\n") . T.strip . H.showTransaction

deriving stock instance Lift H.Side

deriving stock instance Lift H.DigitGroupStyle

deriving stock instance Lift H.AmountPrecision

deriving stock instance Lift H.Rounding

deriving stock instance Lift H.AmountStyle

deriving stock instance Lift H.AmountCost

deriving stock instance Lift H.Amount

deriving stock instance Lift H.MixedAmountKey

deriving stock instance Lift H.MixedAmount

deriving stock instance Lift H.Status

deriving stock instance Lift H.SourcePos

deriving stock instance Lift H.BalanceAssertion

deriving stock instance Lift H.PostingType

instance Lift H.Posting where
  liftTyped = genericLiftTypedCompat

instance Lift H.Transaction where
  -- txnUntieKnot is necessary, because otherwise lifting Transaction is an infinite loop.
  liftTyped = genericLiftTypedCompat . H.txnUntieKnot
