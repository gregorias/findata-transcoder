{-# LANGUAGE OverloadedStrings #-}

-- | This module parses Charles Schwab CSV statement
module Hledupt.CharlesSchwab.Csv (
  -- * Parsing
  parseCsStatement,

  -- * Types
  DollarAmount (..),
  CsCsvRecord (..),
) where

import Control.Lens (over)
import qualified Control.Lens as L
import Control.Monad.Combinators (manyTill)
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (FromNamedRecord (..), (.:))
import qualified Data.Csv as Csv
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time (Day, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import Data.Vector (Vector)
import Hledupt.Data.CsvFile (CsvFile (CsvFile))
import Hledupt.Data.MyDecimal (decimalP)
import Relude
import Text.Megaparsec (Parsec, anySingle, chunk, single)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (signed)
import Text.Megaparsec.Stream (tokensToChunk)

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
  decimalP

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
    let fieldString = Text.pack $ decodeUtf8 field
    maybe
      (fail . Text.unpack $ "Could not parse the dollar amount: " `Text.append` fieldString)
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
      <*> rec .: "Action"
      <*> rec .: "Symbol"
      <*> rec .: "Description"
      <*> quantityP rec
      <*> maybeDollarAmountP rec "Price"
      <*> maybeDollarAmountP rec "Fees & Comm"
      <*> rec .: "Amount"

line :: Parsec Void LBS.ByteString LBS.ByteString
line = do
  (content, eol) <- MP.manyTill_ anySingle (chunk "\n")
  return $ tokensToChunk (Proxy :: Proxy LBS.ByteString) content `LBS.append` eol

dropFirstLine :: Parsec Void LBS.ByteString ()
dropFirstLine = do
  void $ manyTill anySingle (chunk "\n")

lastLine :: Parsec Void LBS.ByteString ()
lastLine = do
  void $ chunk "Transactions Total"
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
      Text.pack
      ( Csv.decodeByName input
      )

parseCsStatement :: LBS.ByteString -> Either Text (Vector CsCsvRecord)
parseCsStatement stmt = do
  csvContent <- over L._Left (Text.pack . MP.errorBundlePretty) $ MP.parse csStatementToCsvContentP "" stmt
  parseCsCsv csvContent
