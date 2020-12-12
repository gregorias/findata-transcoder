{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Hledupt.Ib.Csv
  ( PositionRecordAssetClass (..),
    CashMovement (..),
    Currency (..),
    Dividend (..),
    DividendRecord (..),
    PositionRecord (..),
    Statement (..),
    WithholdingTax (..),
    WithholdingTaxRecord (..),
    parse,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Hledupt.Ib.Csv.CsvParse
  ( CashMovement,
    Currency,
    Dividend,
    DividendRecord,
    PositionRecord,
    PositionRecordAssetClass,
    Statement,
    WithholdingTax,
    WithholdingTaxRecord,
  )
import qualified Hledupt.Ib.Csv.CsvParse as CsvParse
import qualified Hledupt.Ib.Csv.RawParse as RawParse

-- | Parses an M-to-M IB CSV statement into individual data points and records.
parse :: String -> Either String Statement
parse csv = do
  csvs <- first show $ RawParse.parse csv
  CsvParse.parse csvs
