module Hledger.Data.Lens (
  aCommodity,
  aAmountPrice,
  aStyle,
  asCommoditySpaced,
  asPrecision,
  maAmount,
  maMaybeAmount,
  pAccount,
  pAmount,
  pBalanceAssertion,
  pMamount,
  pMaybeAmount,
  pStatus,
  tDescription,
  tStatus,
) where

import Control.Lens (
  Lens',
  Prism',
  Traversal',
  lens,
  prism',
 )
import Data.Text (pack, unpack)
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
pAccount = lens (unpack . paccount) setter
 where
  setter p a = p{paccount = pack a}

pMamount :: Lens' Posting MixedAmount
pMamount = lens pamount setter
 where
  setter p ma = p{pamount = ma}

pStatus :: Lens' Posting Status
pStatus = lens pstatus setter
 where
  setter p s = p{pstatus = s}

maAmount :: Prism' MixedAmount Amount
maAmount = prism' setter getter
 where
  setter = Mixed . (: [])
  getter (Mixed (a : _)) = Just a
  getter (Mixed _) = Nothing

maMaybeAmount :: Lens' MixedAmount (Maybe Amount)
maMaybeAmount = lens getter setter
 where
  setter (Mixed (_ : as)) (Just a') = Mixed (a' : as)
  setter (Mixed (_ : as)) Nothing = Mixed as
  setter (Mixed []) (Just a') = Mixed [a']
  setter (Mixed []) Nothing = Mixed []
  getter (Mixed (a : _)) = Just a
  getter (Mixed _) = Nothing

pAmount :: Traversal' Posting Amount
pAmount = pMamount . maAmount

pMaybeAmount :: Lens' Posting (Maybe Amount)
pMaybeAmount = pMamount . maMaybeAmount

pBalanceAssertion :: Lens' Posting (Maybe BalanceAssertion)
pBalanceAssertion = lens pbalanceassertion setter
 where
  setter p ba = p{pbalanceassertion = ba}

tDescription :: Lens' Transaction String
tDescription = lens (unpack . tdescription) setter
 where
  setter tr description = tr{tdescription = pack description}

tStatus :: Lens' Transaction Status
tStatus = lens tstatus setter
 where
  setter tr st = tr{tstatus = st}
