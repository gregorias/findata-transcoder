{-# LANGUAGE TypeFamilies #-}

-- |
-- This module parses an IB statement (Activities or Mark-to-Market) CSV into
-- its constituent parts.
module Hledupt.Ib.Csv.RawParse
  ( IbCsvs,
    Csv,
    CsvName,
    parse,
  )
where

import Data.List.NonEmpty (groupWith)
import qualified Data.Map.Strict as Map
import Relude
import Text.Megaparsec
  ( Parsec,
    eof,
    errorBundlePretty,
    someTill,
    try,
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, eol, printChar)
import Text.Megaparsec.Char.Extra (bom)

data CsvLine = CsvLine
  { clHeader :: String,
    clBody :: String
  }
  deriving (Eq, Show)

-- | A string containing a CSV file
type Csv = String

-- | A name of a single CSV in an IB statement
type CsvName = String

-- | A collection of CSV files that form subsections in a single IB statement.
type IbCsvs = Map.Map CsvName Csv

rawStatementLineParser :: Parsec Void String CsvLine
rawStatementLineParser = do
  headerString <- someTill printChar (char ',')
  rest <- some printChar
  end <- try eol <|> return ""
  return $ CsvLine headerString (rest ++ end)

groupByHeader :: [CsvLine] -> [(String, NonEmpty String)]
groupByHeader ls =
  map
    (\case a :| as -> (clHeader a, clBody a :| map clBody as))
    (groupWith clHeader ls)

linesToIbCsvs :: [CsvLine] -> IbCsvs
linesToIbCsvs = fmap concat . Map.fromList . groupByHeader

rawStatementParser :: Parsec Void String IbCsvs
rawStatementParser = do
  void $ optional bom
  csvLines <- many rawStatementLineParser
  eof
  return $ linesToIbCsvs csvLines

-- | Parses an IB CSV Statement
parse :: String -> Either String IbCsvs
parse = first ((errorMsg ++) . errorBundlePretty) . MP.parse rawStatementParser ""
  where
    errorMsg = "Could not parse the IB CSV statement.\n"
