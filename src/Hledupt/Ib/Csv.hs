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
    MtmStatement (..),
    Position (..),
    PositionAssetClass (..),
    StockPosition (..),
    Trade (..),
    WithholdingTax (..),

    -- * Parsers
    parseActivity,
    parseMtm,
  )
where

import Hledupt.Ib.Csv.CsvParse
  ( ActivityStatement (..),
    CashMovement,
    Currency,
    Dividend,
    EndingCash (..),
    MtmStatement,
    Position,
    PositionAssetClass,
    StockPosition (..),
    Trade (..),
    WithholdingTax,
    nullActivityStatement,
  )
import qualified Hledupt.Ib.Csv.CsvParse as CsvParse
import qualified Hledupt.Ib.Csv.RawParse as RawParse
import Relude

-- | Parses an M-to-M IB CSV statement into individual data points and records.
parseMtm :: String -> Either String MtmStatement
parseMtm csv = do
  csvs <- RawParse.parse csv
  CsvParse.parseMtmStatement csvs

-- | Parses an Activity IB CSV statement into individual data points and records.
parseActivity :: String -> Either String ActivityStatement
parseActivity csv = do
  csvs <- RawParse.parse csv
  CsvParse.parseActivityStatement csvs
