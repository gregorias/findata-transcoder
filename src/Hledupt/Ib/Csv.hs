{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib.Csv (
  -- * Types
  ActivityStatement (..),
  nullActivityStatement,
  CashMovement (..),
  Dividend (..),
  EndingCash (..),
  StockPosition (..),
  WithholdingTax (..),
  StockTrade (..),
  ForexTrade (..),

  -- * Parsers
  parseActivity,
) where

import qualified Control.Lens as L
import Hledupt.Ib.Csv.ActivityStatementParse (
  ActivityStatement (..),
  CashMovement (..),
  Dividend (..),
  EndingCash (..),
  ForexTrade (..),
  StockPosition (..),
  StockTrade (..),
  WithholdingTax (..),
  nullActivityStatement,
  parseActivityStatement,
 )
import qualified Hledupt.Ib.Csv.RawParse as RawParse
import Relude

-- | Parses an Activity IB CSV statement into individual data points and records.
parseActivity :: Text -> Either Text ActivityStatement
parseActivity csv = L.over L._Left toText $ do
  csvs <- RawParse.parse (toString csv)
  parseActivityStatement csvs
