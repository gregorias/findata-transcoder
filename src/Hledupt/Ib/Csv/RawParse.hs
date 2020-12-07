{-# LANGUAGE TypeFamilies #-}

-- |
-- This module parses original IB Mark-to-Market CSV into its constituent parts.
module Hledupt.Ib.Csv.RawParse
  ( Csvs (..),
    nullcsvs,
    parse,
  )
where

import Text.Megaparsec
  ( MonadParsec,
    ParseErrorBundle,
    Token,
    Tokens,
    eof,
    many,
    optional,
    some,
    someTill,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, eol, printChar)
import Text.Megaparsec.Char.Extra (bom)

data CsvLine = CsvLine
  { clHeader :: String,
    clBody :: String
  }
  deriving (Eq, Show)

data Csvs = Csvs
  { cStatement :: String,
    cPositions :: String,
    cDepositsAndWithdrawals :: String,
    cDividends :: String,
    cWithholdingTax :: String
  }
  deriving (Eq, Show)

nullcsvs :: Csvs
nullcsvs = Csvs "" "" "" "" ""

rawStatementLineParser ::
  (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m CsvLine
rawStatementLineParser = do
  headerString <- someTill printChar (char ',')
  rest <- some printChar
  end <- try eol <|> return ""
  return $ CsvLine headerString (rest ++ end)

rawStatementParser ::
  ( MonadParsec e s m,
    Token s ~ Char,
    Tokens s ~ String
  ) =>
  m Csvs
rawStatementParser = do
  optional bom
  ibCsvLines <- many rawStatementLineParser
  let headerIs header = (== header) . clHeader
      [ stmtLines,
        statusLines,
        cashLines,
        dividendLines,
        withholdingTaxLines
        ] =
          map
            (\header -> filter (headerIs header) ibCsvLines)
            [ "Statement",
              "Positions and Mark-to-Market Profit and Loss",
              "Deposits & Withdrawals",
              "Dividends",
              "Withholding Tax"
            ]
      [stmtCsv, statusCsv, cashCsv, dividendCsv, withholdingTaxCsv] =
        map
          (concatMap clBody)
          [ stmtLines,
            statusLines,
            cashLines,
            dividendLines,
            withholdingTaxLines
          ]
  eof
  return $
    Csvs
      { cStatement = stmtCsv,
        cPositions = statusCsv,
        cDepositsAndWithdrawals = cashCsv,
        cDividends = dividendCsv,
        cWithholdingTax = withholdingTaxCsv
      }

parse :: (Ord e) => String -> Either (ParseErrorBundle String e) Csvs
parse = MP.parse rawStatementParser ""
