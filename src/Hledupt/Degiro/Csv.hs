{-# LANGUAGE OverloadedStrings #-}

-- | This module parses Degiro CSV statement
module Hledupt.Degiro.Csv
  ( -- * Parsing
    parseCsvStatement,

    -- * Types
    DegiroCsvRecord (..),
    Money (..),
    Currency (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import Data.Time (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Vector (Vector)
import Relude

data Currency = EUR | CHF
  deriving stock (Eq, Show)

data Money = Money
  { moneyCurrency :: Currency,
    moneyAmount :: Decimal
  }
  deriving stock (Eq, Show)

data DegiroCsvRecord = DegiroCsvRecord
  { dcrDate :: Day,
    dcrTime :: TimeOfDay,
    dcrValueDate :: Day,
    dcrProduct :: Text,
    dcrIsin :: Text,
    dcrDescription :: Text,
    dcrFx :: Maybe Decimal,
    dcrChange :: Money,
    dcrBalance :: Money,
    dcrOrderId :: Text
  }
  deriving stock (Eq, Show)

instance Csv.FromRecord DegiroCsvRecord where
  parseRecord _rec = do
    fail "FromRecord DegiroCsvRecord is unimplemented"

parseCsvStatement :: LBS.ByteString -> Either String (Vector DegiroCsvRecord)
parseCsvStatement = Csv.decode Csv.HasHeader
