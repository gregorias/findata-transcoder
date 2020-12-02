module Hledger.Data.Lens
  ( aCommodity,
    aStyle,
    asPrecision,
    asCommoditySide,
    asCommoditySpaced,
    maAmount,
    maMaybeAmount,
    pAccount,
    pAmount,
    pMamount,
    pMaybeAmount,
    pBalanceAssertion,
    tDescription,
    tStatus,
  )
where

import Control.Lens
  ( Lens',
    Prism',
    Traversal',
    lens,
    prism',
  )
import Data.Text (pack, unpack)
import Hledger.Data.Types
  ( Amount (..),
    AmountStyle (..),
    BalanceAssertion (..),
    CommoditySymbol,
    MixedAmount (..),
    Posting (..),
    Side (..),
    Status (..),
    Transaction (..),
  )

aCommodity :: Lens' Amount CommoditySymbol
aCommodity = lens acommodity setter
  where
    setter amount ac = amount {acommodity = ac}

aStyle :: Lens' Amount AmountStyle
aStyle = lens astyle setter
  where
    setter amount as = amount {astyle = as}

asPrecision :: Lens' AmountStyle Int
asPrecision = lens asprecision setter
  where
    setter as prec = as {asprecision = prec}

asCommoditySide :: Lens' AmountStyle Side
asCommoditySide = lens ascommodityside setter
  where
    setter as side = as {ascommodityside = side}

asCommoditySpaced :: Lens' AmountStyle Bool
asCommoditySpaced = lens ascommodityspaced setter
  where
    setter as side = as {ascommodityspaced = side}

pAccount :: Lens' Posting String
pAccount = lens (unpack . paccount) setter
  where
    setter p a = p {paccount = pack a}

pMamount :: Lens' Posting MixedAmount
pMamount = lens pamount setter
  where
    setter p ma = p {pamount = ma}

maAmount :: Prism' MixedAmount Amount
maAmount = prism' setter getter
  where
    setter = Mixed . (: [])
    getter (Mixed (a : _)) = Just a
    getter (Mixed _) = Nothing

maMaybeAmount :: Lens' MixedAmount (Maybe Amount)
maMaybeAmount = lens getter setter
  where
    setter (Mixed (a : as)) (Just a') = Mixed (a' : as)
    setter (Mixed (a : as)) Nothing = Mixed as
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
    setter p ba = p {pbalanceassertion = ba}

tDescription :: Lens' Transaction String
tDescription = lens (unpack . tdescription) setter
  where
    setter tr description = tr {tdescription = pack description}

tStatus :: Lens' Transaction Status
tStatus = lens tstatus setter
  where
    setter tr st = tr {tstatus = st}
