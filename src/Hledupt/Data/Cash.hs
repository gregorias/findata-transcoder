{-# LANGUAGE TemplateHaskell #-}

module Hledupt.Data.Cash
  ( Cash (..),
    cashCurrency,
    cashAmount,
  )
where

import Control.Lens (makeLenses)
import Data.Decimal (Decimal)
import Hledupt.Data.Currency (Currency)
import Relude

data Cash = Cash
  { _cashCurrency :: !Currency,
    _cashAmount :: !Decimal
  }
  deriving stock (Eq, Show)

makeLenses ''Cash
