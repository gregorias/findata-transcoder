{-# LANGUAGE TemplateHaskell #-}

module Data.Cash (
  Cash (..),
  cashP,
  cashCurrency,
  cashAmount,
  negate,
) where

import Control.Lens (makeLenses, over)
import Data.Decimal (Decimal)
import Relude hiding (negate)
import qualified Relude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
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
cashP = currencyFirstP <|> currencySecondP
 where
  currencyFirstP = do
    currency <- currencyP
    MP.space1
    amount <- decimalP defaultDecimalFormat
    return $ Cash currency amount
  currencySecondP = do
    amount <- decimalP defaultDecimalFormat
    MP.space1
    currency <- currencyP
    return $ Cash currency amount

negate :: Cash -> Cash
negate = over cashAmount Relude.negate
