module Hledupt.Degiro.IsinData (
  IsinData (..),
  isinToIsinData,
  prettyIsin,
) where

import Hledupt.Data.Currency (Currency, eur)
import Hledupt.Data.Isin (Isin, isin)
import Relude

data IsinData = IsinData
  { isinDataName :: !Text
  , isinDataCurrency :: !Currency
  }

isinToIsinData :: Isin -> Maybe IsinData
isinToIsinData isinArg =
  let iwda = [isin|IE00B4L5Y983|]
      ibgs = [isin|IE00B14X4Q57|]
   in if
          | isinArg == iwda -> Just $ IsinData "IWDA" eur
          | isinArg == ibgs -> Just $ IsinData "IBGS" eur
          | otherwise -> Nothing

prettyIsin :: Isin -> Text
prettyIsin isinArg = maybe (show isinArg) isinDataName (isinToIsinData isinArg)
