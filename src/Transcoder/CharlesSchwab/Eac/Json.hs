-- | This module parses Charles Schwab's EAC history statement JSON into structured records.
module Transcoder.CharlesSchwab.Eac.Json (
  -- * Parsing
  parseHistory,

  -- * JSON Types
  CsDay (..),
  CsDecimal (..),
  WireTransferJson (..),
  WireTransferString (..),
  SaleJson (..),
  SaleString (..),
  DepositJson (..),
  DepositString (..),
) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson hiding (eitherDecodeStrict)
import Data.Aeson.Extra qualified as Aeson
import Data.Decimal qualified as Decimal
import Data.Decimal.Extra qualified as Decimal
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Relude
import Transcoder.CharlesSchwab.DollarAmount (DollarAmount)
import Transcoder.CharlesSchwab.Eac.Data (
  Deposit (..),
  Record (..),
  RecordSheet (..),
  Sale (..),
  WireTransfer (WireTransfer),
 )
import Witch (
  From (from),
  into,
 )

newtype CsDay = CsDay {unCsDay :: Day}
  deriving newtype (Eq, Show)

instance FromJSON CsDay where
  parseJSON = Aeson.withText "MM/DD/YYYY date" $ \text ->
    case parseTimeM False defaultTimeLocale "%m/%d/%Y" (toString text) of
      Nothing -> fail "Invalid date"
      Just day -> return $ CsDay day

newtype CsDecimal = CsDecimal Decimal.Decimal
  deriving newtype (Eq, Show)

instance FromJSON CsDecimal where
  parseJSON =
    coerce
      <$> Decimal.parseJSON
        (Decimal.DecimalFormat (Decimal.ChunkSep ',') (Just Decimal.OptionalUnlimitedDecimalFraction))

-- | A JSON representation of a record sheet.
newtype RecordSheetJson = RecordSheetJson
  { rsjTransactions :: [RecordJson]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RecordSheetJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 3})

instance From RecordSheetJson RecordSheet where
  from (RecordSheetJson{rsjTransactions = trs}) = RecordSheet $ (into @Record) <$> trs

-- records <- mapLeft (nestException rsj) (mapM tryFrom transactions :: Either (TryFromException TransactionJson Record) [Record])
-- return $ RecordSheet records

data RecordJson
  = RecordJsonWireTransfer !WireTransferJson
  | RecordJsonSale !SaleJson
  | RecordJsonDeposit !DepositJson
  deriving stock (Eq, Show, Generic)

instance FromJSON RecordJson where
  parseJSON =
    Aeson.genericParseJSON
      ( Aeson.defaultOptions
          { -- Constructors are recognized by the "action" field.
            Aeson.sumEncoding = Aeson.UntaggedValue
          }
      )

instance From RecordJson Record where
  from = \case
    RecordJsonWireTransfer wireTransferJson -> RecordWireTransfer $ from wireTransferJson
    RecordJsonSale saleJson -> RecordSale $ from saleJson
    RecordJsonDeposit depositJson -> RecordDeposit $ from depositJson

data WireTransferString = WireTransferString
  deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON WireTransferString where
  parseJSON = Aeson.withText "Wire Transfer" $ \text ->
    if text == "Wire Transfer"
      then return WireTransferString
      else fail "Not a wire transfer"

data WireTransferJson = WireTransferJson
  { wtjDate :: !CsDay
  , wtjAction :: !WireTransferString
  , wtjSymbol :: !Text
  , wtjDescription :: !Text
  , wtjAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON WireTransferJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 3})

instance From WireTransferJson WireTransfer where
  from (WireTransferJson day _ symbol description amount) = WireTransfer (coerce day) symbol description amount

data SaleString = SaleString
  deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON SaleString where
  parseJSON = Aeson.withText "Sale" $ \text ->
    if text == "Sale"
      then return SaleString
      else fail $ toString text <> " is not a sale"

data SaleJson = SaleJson
  { sjDate :: !CsDay
  , sjAction :: !SaleString
  , sjSymbol :: !Text
  , sjDescription :: !Text
  , sjQuantity :: !CsDecimal
  , sjFeesAndCommissions :: !DollarAmount
  , sjAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SaleJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 2})

instance From SaleJson Sale where
  from
    ( SaleJson
        { sjDate = date
        , sjSymbol = symbol
        , sjDescription = description
        , sjQuantity = quantity
        , sjFeesAndCommissions = feesAndCommissions
        , sjAmount = amount
        }
      ) =
      Sale
        { sSymbol = symbol
        , sQuantity = coerce quantity
        , sFeesAndCommissions = feesAndCommissions
        , sDescription = description
        , sDate = coerce date
        , sAmount = amount
        }

data DepositString = DepositString
  deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON DepositString where
  parseJSON = Aeson.withText "Deposit" $ \text ->
    if text == "Deposit"
      then return DepositString
      else fail $ toString text <> "is not a deposit"

data DepositJson = DepositJson
  { djDate :: !CsDay
  , djAction :: !DepositString
  , djSymbol :: !Text
  , djDescription :: !Text
  , djQuantity :: !CsDecimal
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON DepositJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 2})

instance From DepositJson Deposit where
  from
    ( DepositJson
        { djDate = date
        , djSymbol = symbol
        , djDescription = description
        , djQuantity = quantity
        }
      ) =
      Deposit
        { dDate = coerce date
        , dSymbol = symbol
        , dQuantity = coerce quantity
        , dDescription = description
        }

parseHistory :: ByteString -> Either Text RecordSheet
parseHistory = Aeson.eitherDecodeStrict @RecordSheetJson >=> (return . into @RecordSheet)
