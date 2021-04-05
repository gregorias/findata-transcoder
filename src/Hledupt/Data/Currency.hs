{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Data.Currency (
  Currency (..),
  currencyP,
) where

import Control.Applicative.Combinators (count)
import qualified Data.Csv as Csv
import Relude
import Relude.Extra (inverseMap)
import Text.Megaparsec (MonadParsec)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Stream (Stream (Token))

-- | 'Currency' represents currencies I'm interested in.
data Currency = CHF | EUR | PLN | USD
  deriving stock (Bounded, Eq, Enum, Ord, Read, Show)

parseCurrency :: Text -> Maybe Currency
parseCurrency = inverseMap show

instance Csv.FromField Currency where
  parseField field =
    case parseCurrency (decodeUtf8 field) of
      Just currency -> return currency
      Nothing -> fail $ "Could not parse the currency: " <> decodeUtf8 field <> "."

currencyP ::
  ( MonadFail m
  , MonadParsec e s m
  , Token s ~ Char
  ) =>
  m Currency
currencyP = do
  Just cur <- parseCurrency . toText <$> count 3 letterChar
  return cur
