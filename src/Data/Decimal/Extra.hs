{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Decimal.Extra () where

import Data.Decimal (DecimalRaw (..))
import Language.Haskell.TH.Syntax (Lift)

deriving stock instance (Lift a) => Lift (DecimalRaw a)
