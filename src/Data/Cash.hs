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
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  decimalP,
 )
import Hledger.Data.Extra (ToAmount (..), makeCurrencyAmount)
import Relude hiding (negate)
import Relude qualified
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP
import Transcoder.Data.Currency (Currency, currencyP)

data Cash = Cash
  { _cashCurrency :: !Currency
  , _cashAmount :: !Decimal
  }
  deriving stock (Eq, Show)

makeLenses ''Cash

instance ToAmount Cash where
  toAmount (Cash currency amount) = makeCurrencyAmount currency amount

-- | Parses cash strings
--
-- >>> MP.parseMaybe cashP "CHF 100.00"
-- Just (Cash {_cashCurrency = CHF, _cashAmount = 100})
--
-- >>> MP.parseMaybe cashP "3.50 USD"
-- Just (Cash {_cashCurrency = USD, _cashAmount = 3.5})
--
-- >>> MP.parseMaybe cashP "19,000 CHF"
-- Just (Cash {_cashCurrency = CHF, _cashAmount = 19000})
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
    <*> toPermutation
      ( decimalP
          ( DecimalFormat
              { decimalFormatFractionFormat = Just OptionalUnlimitedDecimalFraction
              , decimalFormatChunkSep = ChunkSep ','
              }
          )
      )

negate :: Cash -> Cash
negate = over cashAmount Relude.negate
