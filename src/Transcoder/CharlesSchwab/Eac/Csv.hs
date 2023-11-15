-- | This module parses Charles Schwab's EAC history statement CSV into structured records.
module Transcoder.CharlesSchwab.Eac.Csv (
  -- * Parsing
  parseHistory,

  -- * Types
  RecordSheet (..),
  Record (..),
  WireTransfer (..),
  Sale (..),
  SubsaleType (..),
  Subsale (..),
  Deposit (..),
  DepositAwardInfo (..),
) where

import Data.Csv.Extra qualified as Csv
import Data.Decimal (Decimal)
import Data.Time (Day)
import Data.Time.Extra (dayP)
import Relude
import Text.Megaparsec (MonadParsec (eof))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Extra (eolOrEof)
import Text.Megaparsec.Extra (ToNestedParser (toNestedParser))
import Text.Megaparsec.Extra qualified as MP
import Transcoder.CharlesSchwab.Csv (DollarAmount, dollarAmountP)
import Transcoder.Data.MyDecimal (decimalP, defaultDecimalFormat)

-- | A record sheet represent an entire EAC account history statement.
data RecordSheet = RecordSheet
  { rsDate :: !Day
  , rsRecords :: ![Record]
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
  , sSubsales :: ![Subsale]
  }
  deriving stock (Eq, Show, Generic)

data SubsaleType = SubsaleTypeRs
  deriving stock (Eq, Show, Generic)

data Subsale = Subsale
  { sType :: !SubsaleType
  , sShares :: !Decimal
  , sSalePrice :: !DollarAmount
  , sGrantId :: !Text
  , sVestDate :: !Day
  , sVestFMV :: !DollarAmount
  , sGrossProceeds :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

data Deposit = Deposit
  { dDate :: !Day
  , dSymbol :: !Text
  , dDescription :: !Text
  , dQuantity :: !Decimal
  , dDepositAwardInfo :: !DepositAwardInfo
  }
  deriving stock (Eq, Show, Generic)

data DepositAwardInfo = DepositAwardInfo
  { daiAwardDate :: !Day
  , daiAwardId :: !Text
  , daiVestDate :: !Day
  , daiVestFMV :: !DollarAmount
  }
  deriving stock (Eq, Show, Generic)

type Stream = Text

type Parser = MP.Parsec Void Stream

commaP :: Parser ()
commaP = void $ MP.single ','

csDayP :: Parser Day
csDayP = MP.label "MM/DD/YYYY date" $ dayP "%m/%d/%Y"

-- | Nests a parser inside a quoted field parser.
--
-- An EAC account statement is a quasi-CSV file where the fields are always
-- quoted.
nest :: Parser a -> Parser a
nest = MP.nestP @Void Csv.quotedFieldP . toNestedParser . MP.ParsecWithSource "quoted CSV field"

-- "Transaction Details for Equity Awards Center account as of 11/05/2023 05:17:39 AM ET"
preambleP :: Parser Day
preambleP = MP.label "preamble" $ nest textP <* eolOrEof
 where
  textP = string "Transaction Details for Equity Awards Center account as of " *> datetimeP
  datetimeP :: Parser Day
  datetimeP = MP.label "datetime" $ do
    day <- csDayP
    void $ MP.space >> hourP >> MP.space1 >> MP.string "ET"
    return day
   where
    hourP =
      MP.label "hour"
        $ (MP.some MP.digitChar >> MP.single ':' >> MP.some MP.digitChar >> MP.single ':' >> MP.some MP.digitChar)
        >> (MP.space1 >> MP.choice (MP.string <$> ["AM", "PM"]))

headerP :: Parser ()
headerP = MP.label "CSV header" $ do
  void $ string "\"Date\",\"Action\",\"Symbol\",\"Description\",\"Quantity\",\"Fees & Commissions\",\"Disbursement Election\",\"Amount\""
  void eolOrEof

wireTransferP :: Parser WireTransfer
wireTransferP = MP.label "wire transfer" $ do
  date <- MP.try $ do
    date <- nest csDayP
    void $ commaP >> nest (MP.string "Wire Transfer")
    return date
  symbol <-
    toText <$> do
      commaP
      nest $ MP.some MP.letterChar
  description <- commaP *> Csv.quotedFieldP
  void $ MP.string ",\"\",\"\",\"\""
  amount <- commaP *> nest dollarAmountP
  void eolOrEof
  return
    $ WireTransfer
      { wtDate = date
      , wtSymbol = symbol
      , wtDescription = description
      , wtAmount = amount
      }

subSaleP :: Parser Subsale
subSaleP = do
  void headerP
  void $ MP.string "\"\"" <* commaP
  sType <- nest subSaleTypeP <* commaP
  shares <- nest (decimalP defaultDecimalFormat) <* commaP
  salePrice <- nest dollarAmountP <* commaP
  void $ MP.string "\"\",\"\",\"\",\"\",\"\",\"\","
  grantId <- Csv.quotedFieldP <* commaP
  vestDate <- nest csDayP <* commaP
  vestFMV <- nest dollarAmountP <* commaP
  grossProceeds <- nest dollarAmountP <* commaP
  void MP.eol
  return
    Subsale
      { sType = sType
      , sShares = shares
      , sSalePrice = salePrice
      , sGrantId = grantId
      , sVestDate = vestDate
      , sVestFMV = vestFMV
      , sGrossProceeds = grossProceeds
      }
 where
  headerP = MP.label "subsale header" $ MP.string "\"\",\"Type\",\"Shares\",\"Sale Price\",\"Subscription Date\",\"Subscription FMV\",\"Purchase Date\",\"Purchase Price\",\"Purchase FMV\",\"Disposition Type\",\"Grant Id\",\"Vest Date\",\"Vest FMV\",\"Gross Proceeds\"," *> MP.eol
  subSaleTypeP :: Parser SubsaleType
  subSaleTypeP = MP.string "RS" $> SubsaleTypeRs

saleP :: Parser Sale
saleP = MP.label "sale" $ do
  date <- MP.try $ do
    date <- nest csDayP
    void $ commaP >> nest (MP.string "Sale")
    return date
  symbol <- commaP *> Csv.quotedFieldP
  description <- commaP *> Csv.quotedFieldP
  quantity <- commaP *> nest (decimalP defaultDecimalFormat)
  feesAndCommissions <- commaP *> nest dollarAmountP
  void $ MP.string ",\"\""
  amount <- commaP *> nest dollarAmountP
  void MP.eol
  subSales <- MP.many subSaleP
  return
    $ Sale
      { sDate = date
      , sSymbol = symbol
      , sDescription = description
      , sQuantity = quantity
      , sFeesAndCommissions = feesAndCommissions
      , sAmount = amount
      , sSubsales = subSales
      }

depositP :: Parser Deposit
depositP = MP.label "deposit" $ do
  date <- MP.try $ do
    date <- nest csDayP
    void $ commaP >> nest (MP.string "Deposit")
    return date
  symbol <- commaP *> Csv.quotedFieldP
  description <- commaP *> Csv.quotedFieldP
  quantity <- commaP *> nest (decimalP defaultDecimalFormat)
  void $ MP.string ",\"\",\"\",\"\"" *> MP.eol
  dai <- depositAwardInfoP
  return
    $ Deposit
      { dDate = date
      , dSymbol = symbol
      , dDescription = description
      , dQuantity = quantity
      , dDepositAwardInfo = dai
      }
 where
  depositAwardInfoP :: Parser DepositAwardInfo
  depositAwardInfoP = do
    headerP
    void $ MP.string "\"\","
    date <- nest csDayP <* commaP
    awardId <- Csv.quotedFieldP <* commaP
    vestDate <- nest csDayP <* commaP
    vestFMV <- nest dollarAmountP <* commaP
    void eolOrEof
    return
      DepositAwardInfo
        { daiAwardDate = date
        , daiAwardId = awardId
        , daiVestDate = vestDate
        , daiVestFMV = vestFMV
        }
   where
    headerP :: Parser ()
    headerP = MP.label "deposit award info header" $ do
      void $ MP.string "\"\",\"Award Date\",\"Award ID\",\"Vest Date\",\"Vest FMV\"," *> MP.eol

recordP :: Parser Record
recordP =
  MP.choice
    [ RecordWireTransfer <$> wireTransferP
    , RecordSale <$> saleP
    , RecordDeposit <$> depositP
    ]

recordSheetP :: Parser RecordSheet
recordSheetP = do
  preambleDate <- preambleP
  headerP
  records <- MP.many recordP
  eof
  return $ RecordSheet preambleDate records

parseHistory :: Stream -> Either Text RecordSheet
parseHistory = MP.parsePretty recordSheetP "EAC account CSV"
