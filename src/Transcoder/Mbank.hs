module Transcoder.Mbank (
  MbankTransaction (..),
  valueParser,
  mbankCsvParser,
  mTrToLedger,
  mbankCsvToLedger,
  pln,
) where

import Data.Decimal (Decimal)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hledger (missingamt, post)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Posting (
  balassert,
  post',
 )
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types (
  Amount (..),
  Quantity,
  Transaction (..),
 )
import Relude
import Text.Megaparsec (
  Parsec,
  eof,
  noneOf,
  oneOf,
  parse,
 )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Extra (eolOrEof)
import qualified Transcoder.Data.Currency as Currency
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))
import Transcoder.Data.MyDecimal (fromUnitsAndCents)

pln :: Quantity -> Amount
pln = makeCurrencyAmount Currency.pln

-- | mBank's transaction data fetched from their website.
data MbankTransaction = MbankTransaction
  { mTrDate :: Day
  , mTrTitle :: String
  , mTrAmount :: Decimal
  , mTrEndBalance :: Decimal
  }
  deriving stock (Eq, Show)

-- Parser functionality (CSV String → [MbankTransaction])

type MbankParser = Parsec () String

headerParser :: MbankParser ()
headerParser = void (string "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n")

dateParser :: MbankParser Day
dateParser = do
  dateString <- many $ oneOf ("-0123456789" :: [Char])
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

valueParser :: MbankParser Decimal
valueParser = do
  zloteString <- removeSpaces <$> many (oneOf ("-0123456789 " :: [Char]))
  zlote :: Integer <- case readMaybe zloteString of
    Just z -> return z
    Nothing -> fail ""
  groszeString <- (char ',' *> many (oneOf ['0' .. '9']) <* char ' ') <|> pure "00"
  grosze :: Integer <- case readMaybe groszeString of
    Just g -> return g
    Nothing -> fail ""
  void $ string "PLN"
  return $ fromUnitsAndCents zlote grosze
 where
  removeSpaces = filter (' ' /=)

mbankCsvTransactionParser :: MbankParser MbankTransaction
mbankCsvTransactionParser = do
  date <- dateParser <* char ';'
  title <- char '"' *> many (noneOf ['"', ';']) <* char '"' <* char ';'
  void $ many (noneOf [';']) >> char ';'
  void $ many (noneOf [';']) >> char ';'
  value <- valueParser <* char ';'
  balance <- valueParser <* char ';'
  void eolOrEof
  return $ MbankTransaction date title value balance

mbankCsvParser :: MbankParser [MbankTransaction]
mbankCsvParser = do
  headerParser
  many mbankCsvTransactionParser <* eof

-- MbankTransaction to Ledger transformers

sanitizeTitle :: String -> String
sanitizeTitle = beforeTheGap
 where
  beforeTheGap title@(s : ss)
    | take 3 title == "   " = ""
    | otherwise = s : beforeTheGap ss
  beforeTheGap _ = ""

mTrToLedger :: MbankTransaction -> Transaction
mTrToLedger mTr = tr{tdescription = toText $ sanitizeTitle $ mTrTitle mTr}
 where
  tr =
    transaction
      (mTrDate mTr)
      [ post' "Assets:Liquid:mBank" (pln $ mTrAmount mTr) (balassert $ pln $ mTrEndBalance mTr)
      , post "Expenses:Other" missingamt
      ]

mbankCsvToLedger :: String -> Either Text LedgerReport
mbankCsvToLedger inputCsv = do
  let parserErrorToString err =
        "Could not parse mBank's CSV.\n" <> show err
  mtransactions <- first parserErrorToString $ parse mbankCsvParser "" inputCsv
  let sortedMTransactions = sortOn mTrDate mtransactions
      ltransactions = fmap mTrToLedger sortedMTransactions
  return $ LedgerReport ltransactions []
