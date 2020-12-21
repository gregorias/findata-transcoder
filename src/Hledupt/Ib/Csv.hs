{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib.Csv
  ( -- * Types
    ActivityStatement (..),
    nullActivityStatement,
    CashMovement (..),
    Currency (..),
    Dividend (..),
    EndingCash (..),
    StockPosition (..),
    WithholdingTax (..),
    StockTrade (..),
    ForexTrade (..),

    -- * Parsers
    parseActivity,
  )
where

import Hledupt.Ib.Csv.ActivityStatementParse
  ( ActivityStatement (..),
    CashMovement (..),
    Currency (..),
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
parseActivity :: String -> Either String ActivityStatement
parseActivity csv = do
  csvs <- RawParse.parse csv
  parseActivityStatement csvs
