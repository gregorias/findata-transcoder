-- | Utilities for parsing Charles Schwab's brokerage account history CSV.
module Transcoder.CharlesSchwab.Brokerage.Csv (
  -- * Parsing
  parseCsStatement,

  -- * Types
  CsCsvRecord (..),
) where

import Control.Lens qualified as L
import Control.Monad.Combinators (manyTill)
import Data.ByteString.Lazy qualified as LBS
import Data.Csv (FromNamedRecord (..), (.:))
import Data.Csv qualified as Csv
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Data.Vector (Vector)
import Relude
import Text.Megaparsec (Parsec, anySingle, chunk)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Text.Megaparsec.Stream (tokensToChunk)
import Transcoder.CharlesSchwab.Csv (DollarAmount, dollarAmountP)
import Transcoder.Data.CsvFile (CsvFile (..))

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

quantityP :: Csv.NamedRecord -> Csv.Parser (Maybe Integer)
quantityP rec = do
  field <- rec .: "Quantity"
  if field == ""
    then pure Nothing
    else return $ readMaybe field

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

type Parser = Parsec Void LBS.ByteString

maybeDollarAmountP :: Csv.NamedRecord -> ByteString -> Csv.Parser (Maybe DollarAmount)
maybeDollarAmountP rec name = do
  field <- rec .: name
  if field == ""
    then pure Nothing
    else return $ MP.parseMaybe dollarAmountP field

line :: Parser LBS.ByteString
line = do
  (content, eol) <- MP.manyTill_ anySingle (chunk "\n")
  return $ tokensToChunk (Proxy :: Proxy LBS.ByteString) content `LBS.append` eol

csStatementToCsvContentP :: Parser (CsvFile LBS.ByteString)
csStatementToCsvContentP = do
  csvLines <- many line
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
