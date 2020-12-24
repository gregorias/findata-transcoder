{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Data.Currency
  ( Currency (..),
    currencyP,
  )
where

import Control.Applicative.Combinators (count)
import Data.ByteString.UTF8 as UTF8
import qualified Data.Csv as Csv
import Relude
import Text.Megaparsec (MonadParsec)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Stream (Stream (Token))

-- | 'Currency' represents currencies I'm interested in.
data Currency = CHF | EUR | PLN | USD
  deriving stock (Eq, Enum, Read, Show)

instance Csv.FromField Currency where
  parseField "CHF" = return CHF
  parseField "EUR" = return EUR
  parseField "PLN" = return PLN
  parseField "USD" = return USD
  parseField field =
    fail $
      "Could not parse the currency: " ++ UTF8.toString field ++ "."

currencyP ::
  ( MonadFail m,
    MonadParsec e s m,
    Token s ~ Char
  ) =>
  m Currency
currencyP = do
  Just cur <- readMaybe <$> count 3 letterChar
  return cur
