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
    ActivityStatement (..),
    MtmStatement (..),
    WithholdingTax (..),
    parse,
  )
where

import Hledupt.Ib.Csv.CsvParse
  ( ActivityStatement,
    CashMovement,
    Currency,
    Dividend,
    MtmStatement,
    Position,
    PositionAssetClass,
    WithholdingTax,
  )
import qualified Hledupt.Ib.Csv.CsvParse as CsvParse
import qualified Hledupt.Ib.Csv.RawParse as RawParse

-- | Parses an M-to-M IB CSV statement into individual data points and records.
parse :: String -> Either String MtmStatement
parse csv = do
  csvs <- RawParse.parse csv
  CsvParse.parseMtmStatement csvs
