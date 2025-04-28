{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Hledger.Data.Lens (
  aCommodity,
  aAmountCost,
  aStyle,
  asCommoditySpaced,
  asPrecision,
  baAmount,
  maAmount,
  maAmounts,
  pAccount,
  pAmounts,
  pAmount,
  pBalanceAssertion,
  pComment,
  pMamount,
  pMaybeAmount,
  pStatus,
  tDescription,
  tStatus,
  tPostings,
) where

import Control.Lens (
  Lens',
  Prism',
  Setter',
  Traversal',
  lens,
  mapped,
  over,
  prism',
  sets,
 )
import Data.Map.Strict (assocs, elems)
import Hledger (AmountCost (TotalCost, UnitCost))
import Hledger.Data.Types (
  Amount (..),
  AmountPrecision,
  AmountStyle (..),
  BalanceAssertion (..),
  CommoditySymbol,
  MixedAmount (..),
  MixedAmountKey (MixedAmountKeyNoCost, MixedAmountKeyTotalCost, MixedAmountKeyUnitCost),
  Posting (..),
  Status (..),
  Transaction (..),
 )
import Relude

aAmountCost :: Lens' Amount (Maybe AmountCost)
aAmountCost = lens acost setter
 where
  setter a ac = a{acost = ac}

aCommodity :: Lens' Amount CommoditySymbol
aCommodity = lens acommodity setter
 where
  setter amount ac = amount{acommodity = ac}

aStyle :: Lens' Amount AmountStyle
aStyle = lens astyle setter
 where
  setter amount as = amount{astyle = as}

asPrecision :: Lens' AmountStyle AmountPrecision
asPrecision = lens asprecision setter
 where
  setter as prec = as{asprecision = prec}

asCommoditySpaced :: Lens' AmountStyle Bool
asCommoditySpaced = lens ascommodityspaced setter
 where
  setter as side = as{ascommodityspaced = side}

pAccount :: Lens' Posting String
pAccount = lens (toString . paccount) setter
 where
  setter p a = p{paccount = toText a}

pComment :: Lens' Posting Text
pComment = lens pcomment setter
 where
  setter p a = p{pcomment = a}

pMamount :: Lens' Posting MixedAmount
pMamount = lens pamount setter
 where
  setter p ma = p{pamount = ma}

pStatus :: Lens' Posting Status
pStatus = lens pstatus setter
 where
  setter p s = p{pstatus = s}

baAmount :: Lens' BalanceAssertion Amount
baAmount = lens baamount setter
 where
  setter ba amt = ba{baamount = amt}

amountToMixedAmountKey :: Amount -> MixedAmountKey
amountToMixedAmountKey (Amount comm _ _ Nothing) = MixedAmountKeyNoCost comm
amountToMixedAmountKey (Amount comm _ _ (Just ap)) =
  case ap of
    UnitCost (Amount comm' q _ _) -> MixedAmountKeyUnitCost comm comm' q
    TotalCost (Amount comm' _ _ __) -> MixedAmountKeyTotalCost comm comm'

maAmount :: Prism' MixedAmount Amount
maAmount = prism' setter getter
 where
  setter amount = Mixed $ one (amountToMixedAmountKey amount, amount)
  getter (Mixed m) = case elems m of
    (a : _) -> Just a
    [] -> Nothing

maAmounts :: Lens' MixedAmount [Amount]
maAmounts = lens getter setter
 where
  setter _ as = Mixed . fromList $ (\a -> (amountToMixedAmountKey a, a)) <$> as
  getter (Mixed as) = elems as

maMaybeAmount :: Lens' MixedAmount (Maybe Amount)
maMaybeAmount = lens getter setter
 where
  setter (Mixed m) maybeAmount =
    case assocs m of
      [] -> Mixed . fromList $ (\a -> (amountToMixedAmountKey a, a)) <$> maybeToList maybeAmount
      (_ : kas) -> case maybeAmount of
        Nothing -> Mixed . fromList $ kas
        Just a -> Mixed . fromList $ (amountToMixedAmountKey a, a) : kas
  getter (Mixed m) = listToMaybe $ elems m

pAmounts :: Setter' Posting Amount
pAmounts = sets setter
 where
  setter :: (Amount -> Amount) -> Posting -> Posting
  setter f = balanceAssertionSetter f . mixedAmountSetter f
  mixedAmountSetter = over (pMamount . maAmounts . mapped)
  balanceAssertionSetter = over (pBalanceAssertion . mapped . baAmount)

pAmount :: Traversal' Posting Amount
pAmount = pMamount . maAmount

pMaybeAmount :: Lens' Posting (Maybe Amount)
pMaybeAmount = pMamount . maMaybeAmount

pBalanceAssertion :: Lens' Posting (Maybe BalanceAssertion)
pBalanceAssertion = lens pbalanceassertion setter
 where
  setter p ba = p{pbalanceassertion = ba}

tDescription :: Lens' Transaction Text
tDescription = lens tdescription setter
 where
  setter tr description = tr{tdescription = description}

tPostings :: Lens' Transaction [Posting]
tPostings = lens tpostings setter
 where
  setter tr postings = tr{tpostings = postings}

tStatus :: Lens' Transaction Status
tStatus = lens tstatus setter
 where
  setter tr st = tr{tstatus = st}
