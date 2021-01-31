{-# LANGUAGE OverloadedStrings #-}

-- | This module parses Charles Schwab CSV statement
module Hledupt.CharlesSchwab.Csv (
  -- * Parsing
  parseCsStatement,

  -- * Types
  CsCsvRecord (..),
) where

import Control.Lens (over)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Vector (Vector)
import Hledupt.Data.Cash (Cash)
import Hledupt.Data.CsvFile (CsvFile)
import Relude
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP

data CsCsvRecord = CsCsvRecord
  { csDate :: !Day
  , csAction :: !Text
  , csSymbol :: !Text
  , csDescription :: !Text
  , csQuantity :: !Integer
  , csPrice :: !Decimal
  , csFees :: !Cash
  , csAmount :: !Cash
  }
  deriving stock (Eq, Ord, Show)

csStatementToCsvContentP :: Parsec Void LBS.ByteString (CsvFile LBS.ByteString)
csStatementToCsvContentP = fail "Unimplemented"

parseCsCsv :: CsvFile LBS.ByteString -> Either Text (Vector CsCsvRecord)
parseCsCsv _ = Left "Unimplemented"

parseCsStatement :: LBS.ByteString -> Either Text (Vector CsCsvRecord)
parseCsStatement stmt = do
  csvContent <- over L._Left (Text.pack . MP.errorBundlePretty) $ MP.parse csStatementToCsvContentP "" stmt
  parseCsCsv csvContent
