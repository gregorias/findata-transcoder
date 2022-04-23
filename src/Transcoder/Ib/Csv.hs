module Transcoder.Ib.Csv (
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

import Transcoder.Ib.Csv.ActivityStatementParse (
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
import qualified Transcoder.Ib.Csv.RawParse as RawParse
import Relude

-- | Parses an Activity IB CSV statement into individual data points and records.
parseActivity :: Text -> Either Text ActivityStatement
parseActivity csv = do
  csvs <- RawParse.parse csv
  parseActivityStatement csvs
