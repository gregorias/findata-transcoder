-- | The fundamental data types for EAC account history statements.
--
-- EAC history statements can be in CSV, JSON or other formats. These data
-- types provide a unified representation of the data regardless of the format.
module Transcoder.CharlesSchwab.Eac.Data (
  -- * Types
  RecordSheet (..),
  Record (..),
  WireTransfer (..),
  Sale (..),
  Deposit (..),
) where

import Data.Decimal (Decimal)
import Data.Time (Day)
import Relude
import Transcoder.CharlesSchwab.DollarAmount (DollarAmount)

-- | A record sheet represents an entire EAC account history statement.
newtype RecordSheet = RecordSheet
  { rsRecords :: [Record]
  }
  deriving stock (Eq, Show, Generic)

-- | A record represents a single transaction in an EAC account history
-- statement.
data Record
  = RecordWireTransfer !WireTransfer
  | RecordSale !Sale
  | RecordDeposit !Deposit
  deriving stock (Eq, Show, Generic)

data WireTransfer = WireTransfer
  { wtDate :: !Day
  , wtSymbol :: !Text
  , wtDescription :: !Text
  , wtAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data Sale = Sale
  { sDate :: !Day
  , sSymbol :: !Text
  , sDescription :: !Text
  , sQuantity :: !Decimal
  , sFeesAndCommissions :: !DollarAmount
  , sAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data Deposit = Deposit
  { dDate :: !Day
  , dSymbol :: !Text
  , dDescription :: !Text
  , dQuantity :: !Decimal
  }
  deriving stock (Eq, Show, Generic)
