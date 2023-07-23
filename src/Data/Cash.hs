{-# LANGUAGE TemplateHaskell #-}

module Data.Cash (
  Cash (..),
  cashP,
  cashCurrency,
  cashAmount,
  negate,
) where

import Control.Lens (makeLenses, over)
import Control.Monad.Permutations (toPermutation)
import Control.Monad.Permutations qualified as Permutations
import Data.Decimal (Decimal)
import Relude hiding (negate)
import Relude qualified
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Transcoder.Data.Currency (Currency, currencyP)
import Transcoder.Data.MyDecimal (decimalP, defaultDecimalFormat)

data Cash = Cash
  { _cashCurrency :: !Currency
  , _cashAmount :: !Decimal
  }
  deriving stock (Eq, Ord, Show)

makeLenses ''Cash

-- | Parses cash strings
--
-- >>> MP.parseMaybe cashP "CHF 100.00"
-- Just (Cash {_cashCurrency = CHF, _cashAmount = 100})
--
-- >>> MP.parseMaybe cashP "3.50 USD"
-- Just (Cash {_cashCurrency = USD, _cashAmount = 3.50})
cashP ::
  ( MonadFail m
  , MP.MonadParsec e s m
  , MP.Token s ~ Char
  ) =>
  m Cash
cashP =
  Permutations.intercalateEffect MP.space1
    $ Cash
    <$> toPermutation currencyP
    <*> toPermutation (decimalP defaultDecimalFormat)

negate :: Cash -> Cash
negate = over cashAmount Relude.negate
