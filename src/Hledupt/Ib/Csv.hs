{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib.Csv
  ( PositionAssetClass (..),
    CashMovement (..),
    Currency (..),
    Dividend (..),
    Position (..),
    Statement (..),
    WithholdingTax (..),
    parse,
  )
where

import Hledupt.Ib.Csv.CsvParse
  ( CashMovement,
    Currency,
    Dividend,
    Position,
    PositionAssetClass,
    Statement,
    WithholdingTax,
  )
import qualified Hledupt.Ib.Csv.CsvParse as CsvParse
import qualified Hledupt.Ib.Csv.RawParse as RawParse

-- | Parses an M-to-M IB CSV statement into individual data points and records.
parse :: String -> Either String Statement
parse csv = do
  csvs <- RawParse.parse csv
  CsvParse.parseMtmStatement csvs
