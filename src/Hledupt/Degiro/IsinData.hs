{-# LANGUAGE OverloadedStrings #-}

module Hledupt.Degiro.IsinData (
  IsinData (..),
  isinToIsinData,
  prettyIsin,
) where

import Hledupt.Data.Currency (Currency (EUR))
import Hledupt.Data.Isin (Isin, mkIsin)
import Relude

data IsinData = IsinData
  { isinDataName :: Text
  , isinDataCurrency :: Currency
  }

isinToIsinData :: Isin -> Maybe IsinData
isinToIsinData isin =
  let iwda = mkIsin "IE00B4L5Y983"
      ibgs = mkIsin "IE00B14X4Q57"
   in if
          | Just isin == iwda -> Just $ IsinData "IWDA" EUR
          | Just isin == ibgs -> Just $ IsinData "IBGS" EUR
          | otherwise -> Nothing

prettyIsin :: Isin -> Text
prettyIsin isin = maybe (show isin) isinDataName (isinToIsinData isin)
