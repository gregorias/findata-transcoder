{-# LANGUAGE TemplateHaskell #-}

module Hledupt.Data.Cash
  ( Cash (..),
    cashCurrency,
    cashAmount,
    negate,
  )
where

import Control.Lens (makeLenses, over)
import Data.Decimal (Decimal)
import Hledupt.Data.Currency (Currency)
import Relude hiding (negate)
import qualified Relude

data Cash = Cash
  { _cashCurrency :: !Currency,
    _cashAmount :: !Decimal
  }
  deriving stock (Eq, Ord, Show)

makeLenses ''Cash

negate :: Cash -> Cash
negate = over cashAmount Relude.negate
