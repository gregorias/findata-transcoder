-- | This module parses Charles Schwab CSV statement
module Transcoder.CharlesSchwab.Csv (
  -- * Parsing
  parseCsStatement,
  dollarAmountP,

  -- * Types
  DollarAmount (..),
  CsCsvRecord (..),
) where

import Control.Lens qualified as L
import Control.Monad.Combinators (manyTill)
import Data.ByteString.Lazy qualified as LBS
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Data.Vector (Vector)
import Relude
import Text.Megaparsec (Parsec, anySingle, chunk, single)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (signed)
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Text.Megaparsec.Stream (tokensToChunk)
import Transcoder.Data.CsvFile (CsvFile (CsvFile))
import Transcoder.Data.MyDecimal (
  decimalP,
  defaultDecimalFormat,
 )

csSimpleDayP :: Parsec Void String Day
csSimpleDayP = do
  content <- many anySingle
  parseTimeM True defaultTimeLocale "%m/%d/%Y" content

csAsOfDayP :: Parsec Void String Day
csAsOfDayP = do
  content <- manyTill anySingle (MP.single ' ')
  void MP.takeRest
  parseTimeM True defaultTimeLocale "%m/%d/%Y" content

csDayP :: Parsec Void String Day
csDayP = MP.try csSimpleDayP <|> csAsOfDayP

newtype CsDay = CsDay
  { unCsDay :: Day
  }

instance Csv.FromField CsDay where
  parseField field = do
    let (day :: Maybe Day) = MP.parseMaybe csDayP (decodeUtf8 field)
    maybe (fail $ "Could not parse the date field: " ++ decodeUtf8 field) (return . CsDay) day

-- I would not need this type with dependent types. I would just use (Cash CHF)
newtype DollarAmount = DollarAmount
  { unDollarAmount :: Decimal
  }
  deriving newtype (Eq, Ord, Show)

unsignedDollarAmountP :: Parsec Void Text Decimal
unsignedDollarAmountP = do
  void $ single '$'
  decimalP defaultDecimalFormat

dollarAmountP :: Parsec Void Text DollarAmount
dollarAmountP = DollarAmount <$> signed space unsignedDollarAmountP

quantityP :: Csv.NamedRecord -> Csv.Parser (Maybe Integer)
quantityP rec = do
  field <- rec .: "Quantity"
  if field == ""
    then pure Nothing
    else return $ readMaybe field

maybeDollarAmountP :: Csv.NamedRecord -> ByteString -> Csv.Parser (Maybe DollarAmount)
maybeDollarAmountP rec name = do
  field <- rec .: name
  if field == ""
    then pure Nothing
    else return $ MP.parseMaybe dollarAmountP field

instance Csv.FromField DollarAmount where
  parseField field = do
    let fieldString = decodeUtf8 field
    maybe
      (fail . toString $ "Could not parse the dollar amount: " <> fieldString)
      return
      (MP.parseMaybe dollarAmountP fieldString)

data CsCsvRecord = CsCsvRecord
  { csDate :: !Day
  , csAction :: !Text
  , csSymbol :: !Text
  , csDescription :: !Text
  , csQuantity :: !(Maybe Integer)
  , csPrice :: !(Maybe DollarAmount)
  , csFees :: !(Maybe DollarAmount)
  , csAmount :: !(Maybe DollarAmount)
  }
  deriving stock (Eq, Ord, Show)

instance FromNamedRecord CsCsvRecord where
  parseNamedRecord rec =
    CsCsvRecord
      <$> (unCsDay <$> rec .: "Date")
      <*> rec
      .: "Action"
      <*> rec
      .: "Symbol"
      <*> rec
      .: "Description"
      <*> quantityP rec
      <*> maybeDollarAmountP rec "Price"
      <*> maybeDollarAmountP rec "Fees & Comm"
      <*> rec
      .: "Amount"

line :: Parsec Void LBS.ByteString LBS.ByteString
line = do
  (content, eol) <- MP.manyTill_ anySingle (chunk "\n")
  return $ tokensToChunk (Proxy :: Proxy LBS.ByteString) content `LBS.append` eol

dropFirstLine :: Parsec Void LBS.ByteString ()
dropFirstLine = do
  void $ manyTill anySingle (chunk "\n")

lastLine :: Parsec Void LBS.ByteString ()
lastLine = do
  void $ chunk "Transactions Total" <|> chunk "\"Transactions Total\""
  void MP.takeRest

csStatementToCsvContentP :: Parsec Void LBS.ByteString (CsvFile LBS.ByteString)
csStatementToCsvContentP = do
  dropFirstLine
  csvLines <- manyTill line (MP.try lastLine)
  return $ CsvFile (LBS.concat csvLines)

parseCsCsv :: CsvFile LBS.ByteString -> Either Text (Vector CsCsvRecord)
parseCsCsv (CsvFile input) =
  snd
    <$> L.over
      L._Left
      toText
      ( Csv.decodeByName input
      )

parseCsStatement :: LBS.ByteString -> Either Text (Vector CsCsvRecord)
parseCsStatement stmt = do
  csvContent <- parsePretty csStatementToCsvContentP "Charles Schwab Statement" stmt
  parseCsCsv csvContent
