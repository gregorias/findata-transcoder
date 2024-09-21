-- | This module parses Charles Schwab's EAC history statement JSON into structured records.
module Transcoder.CharlesSchwab.Eac.Json (
  -- * Parsing
  parseHistory,

  -- * JSON Types
  WireTransferJson (..),
  WireTransferString (..),
  SaleJson (..),
  SaleString (..),
  DepositJson (..),
  DepositString (..),
  CsDay (..),
  CsDecimal (..),
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
  Dividend (..),
  Record (..),
  RecordSheet (..),
  Sale (..),
  TaxWithholding (..),
  WireTransfer (WireTransfer),
 )
import Witch (
  From (from),
  into,
 )

parseHistory :: ByteString -> Either Text RecordSheet
parseHistory = Aeson.eitherDecodeStrict @RecordSheetJson >=> (return . into @RecordSheet)

-- | A JSON representation of a record sheet.
newtype RecordSheetJson = RecordSheetJson
  { rsjTransactions :: [RecordJson]
  }
  deriving stock (Eq, Show, Generic)

data RecordJson
  = RecordJsonWireTransfer !WireTransferJson
  | RecordJsonSale !SaleJson
  | RecordJsonDeposit !DepositJson
  | RecordJsonDividend !DividendJson
  | RecordJsonTaxWithholding !TaxWithholdingJson
  deriving stock (Eq, Show, Generic)

data WireTransferJson = WireTransferJson
  { wtjDate :: !CsDay
  , wtjAction :: !WireTransferString
  , wtjSymbol :: !Text
  , wtjDescription :: !Text
  , wtjAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data WireTransferString = WireTransferString
  deriving stock (Eq, Show, Generic, Typeable)

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

data SaleString = SaleString
  deriving stock (Eq, Show, Generic, Typeable)

data DepositJson = DepositJson
  { djDate :: !CsDay
  , djAction :: !DepositString
  , djSymbol :: !Text
  , djDescription :: !Text
  , djQuantity :: !CsDecimal
  }
  deriving stock (Eq, Show, Generic)

data DepositString = DepositString
  deriving stock (Eq, Show, Generic, Typeable)

data DividendJson = DividendJson
  { dividendJsonDate :: !CsDay
  , dividendJsonAction :: !DividendString
  , dividendJsonSymbol :: !Text
  , dividendJsonAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data DividendString = DividendString
  deriving stock (Eq, Show, Generic, Typeable)

data TaxWithholdingJson = TaxWithholdingJson
  { taxWithholdingJsonDate :: !CsDay
  , taxWithholdingJsonAction :: !TaxWithholdingString
  , taxWithholdingJsonSymbol :: !Text
  , taxWithholdingJsonAmount :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data TaxWithholdingString = TaxWithholdingString
  deriving stock (Eq, Show, Generic, Typeable)

newtype CsDay = CsDay {unCsDay :: Day}
  deriving newtype (Eq, Show)

newtype CsDecimal = CsDecimal Decimal.Decimal
  deriving newtype (Eq, Show)

instance FromJSON CsDay where
  parseJSON = Aeson.withText "MM/DD/YYYY date" $ \text ->
    case parseTimeM False defaultTimeLocale "%m/%d/%Y" (toString text) of
      Nothing -> fail "Invalid date"
      Just day -> return $ CsDay day

instance FromJSON CsDecimal where
  parseJSON =
    coerce
      <$> Decimal.parseJSON
        (Decimal.DecimalFormat (Decimal.ChunkSep ',') (Just Decimal.OptionalUnlimitedDecimalFraction))

instance FromJSON RecordSheetJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 3})

instance From RecordSheetJson RecordSheet where
  from (RecordSheetJson{rsjTransactions = trs}) = RecordSheet $ (into @Record) <$> trs

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
    RecordJsonDividend dividendJson -> RecordDividend $ from dividendJson
    RecordJsonTaxWithholding taxWithholdingJson -> RecordTaxWithholding $ from taxWithholdingJson

instance FromJSON WireTransferString where
  parseJSON = Aeson.withText "Wire Transfer" $ \text ->
    if text == "Wire Transfer"
      then return WireTransferString
      else fail "Not a wire transfer"

instance FromJSON WireTransferJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 3})

instance From WireTransferJson WireTransfer where
  from (WireTransferJson day _ symbol description amount) = WireTransfer (coerce day) symbol description amount

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

instance FromJSON SaleString where
  parseJSON = Aeson.withText "Sale" $ \text ->
    if text == "Sale"
      then return SaleString
      else fail $ toString text <> " is not a sale"

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

instance FromJSON DepositString where
  parseJSON = Aeson.withText "Deposit" $ \text ->
    if text == "Deposit"
      then return DepositString
      else fail $ toString text <> " is not a deposit"

instance FromJSON DividendJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 12})

instance From DividendJson Dividend where
  from
    ( DividendJson
        { dividendJsonDate = date
        , dividendJsonSymbol = symbol
        , dividendJsonAmount = amount
        }
      ) =
      Dividend
        { dividendDate = coerce date
        , dividendSymbol = symbol
        , dividendAmount = amount
        }

instance FromJSON DividendString where
  parseJSON = Aeson.withText "Dividend" $ \text ->
    if text == "Dividend"
      then return DividendString
      else fail $ toString text <> " is not a dividend"

instance FromJSON TaxWithholdingJson where
  parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 18})

instance From TaxWithholdingJson TaxWithholding where
  from
    ( TaxWithholdingJson
        { taxWithholdingJsonDate = date
        , taxWithholdingJsonSymbol = symbol
        , taxWithholdingJsonAmount = amount
        }
      ) =
      TaxWithholding
        { taxWithholdingDate = coerce date
        , taxWithholdingSymbol = symbol
        , taxWithholdingAmount = amount
        }

instance FromJSON TaxWithholdingString where
  parseJSON = Aeson.withText "Tax Withholding" $ \text ->
    if text == "Tax Withholding"
      then return TaxWithholdingString
      else fail $ toString text <> " is not a Tax WIthholding"
