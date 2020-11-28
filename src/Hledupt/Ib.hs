{-# LANGUAGE ScopedTypeVariables #-}

module Hledupt.Ib
  ( ibCsvToLedger,
    IbCsvs (..),
    rawStatementParser,
  )
where

import Text.Megaparsec
  ( Parsec,
  )

-- Parsing (Raw Account Statement → IbCsvs)

type Csv = String

data IbCsvs = IbCsvs
  { statement :: Csv,
    positions :: Csv
  }
  deriving (Eq, Show)

type IbParser = Parsec () String

rawStatementParser :: IbParser IbCsvs
rawStatementParser = undefined

ibCsvToLedger :: String -> String
ibCsvToLedger _ = "Could not parse the CSV, because the functionality is unimplemented"
