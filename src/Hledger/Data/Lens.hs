module Hledger.Data.Lens (
  aCommodity,
  aAmountPrice,
  aStyle,
  asCommoditySpaced,
  asPrecision,
  baAmount,
  maAmount,
  maAmounts,
  maMaybeAmount,
  pAccount,
  pAmounts,
  pAmount,
  pBalanceAssertion,
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
import Hledger (AmountPrice)
import Hledger.Data.Types (
  Amount (..),
  AmountPrecision,
  AmountStyle (..),
  BalanceAssertion (..),
  CommoditySymbol,
  MixedAmount (..),
  Posting (..),
  Status (..),
  Transaction (..),
 )
import Relude

aAmountPrice :: Lens' Amount (Maybe AmountPrice)
aAmountPrice = lens aprice setter
 where
  setter a ap = a{aprice = ap}

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

maAmount :: Prism' MixedAmount Amount
maAmount = prism' setter getter
 where
  setter = Mixed . (: [])
  getter (Mixed (a : _)) = Just a
  getter (Mixed _) = Nothing

maAmounts :: Lens' MixedAmount [Amount]
maAmounts = lens getter setter
 where
  setter _ = Mixed
  getter (Mixed as) = as

maMaybeAmount :: Lens' MixedAmount (Maybe Amount)
maMaybeAmount = lens getter setter
 where
  setter (Mixed (_ : as)) (Just a') = Mixed (a' : as)
  setter (Mixed (_ : as)) Nothing = Mixed as
  setter (Mixed []) (Just a') = Mixed [a']
  setter (Mixed []) Nothing = Mixed []
  getter (Mixed (a : _)) = Just a
  getter (Mixed _) = Nothing

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
