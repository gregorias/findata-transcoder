module Hledger.Data.Lens where

import           Control.Lens       (Lens', lens)
import           Hledger.Data.Types (Amount (..), AmountStyle (..), Side (..))

aStyle :: Lens' Amount AmountStyle
aStyle = lens astyle setter
  where
  setter amount as = amount{astyle=as}

asPrecision :: Lens' AmountStyle Int
asPrecision = lens asprecision setter
  where
  setter as prec = as{asprecision=prec}

asCommoditySide :: Lens' AmountStyle Side
asCommoditySide = lens ascommodityside setter
  where
  setter as side = as{ascommodityside=side}

asCommoditySpaced :: Lens' AmountStyle Bool
asCommoditySpaced = lens ascommodityspaced setter
  where
  setter as side = as{ascommodityspaced=side}
