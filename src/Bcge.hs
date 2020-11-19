{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}
module Bcge(
bcgeCsvToLedger,
  BcgeTransaction(..),
  CsvLine(..),
  csvLineToBcgeTransaction,
  parseStatementDate,
  statementDateParser,
  saldoToLedger
)
where

import           Control.Lens              (makeLenses, over, set, (&), (.~),
                                            (^.))
import           Control.Monad             (void)
import           Control.Monad.Writer.Lazy (execWriter, tell)
import           Data                      (MonetaryValue, fromUnitsAndCents)
import           Data.List                 (elemIndex, isInfixOf)
import           Data.Time.Calendar        (Day)
import           Data.Time.Format          (defaultTimeLocale, parseTimeM)
import           Hledger.Data.Amount       (amountWithCommodity, num)
import           Hledger.Data.Posting      (balassert, nullposting, post')
import           Hledger.Data.Transaction  (showTransaction, transaction)
import           Hledger.Data.Types        (Amount (..), AmountStyle (..),
                                            Posting (..), Quantity (..),
                                            Side (..), Status (..),
                                            Transaction (..))
import           Overhang                  (onNothing)
import           Safe                      (headMay)
import           Text.Megaparsec           (Parsec, eof, many, noneOf, oneOf,
                                            parse, parseMaybe, some, (<|>))
import           Text.Megaparsec.Char      (char, eol, string)

import qualified Hledger.Data.Extra        as HDE
import           Hledger.Data.Lens         (aStyle, asCommoditySide,
                                            asCommoditySpaced, asPrecision,
                                            pAccount, pBalanceAssertion,
                                            tDescription, tStatus)


bcgeAccount = "Assets:Liquid:BCGE"

-- Parser functionality (CSV String → [BcgeTransaction])

type CsvLine = (String, String, String)
type Header = [CsvLine]
type Content = [CsvLine]
type BcgeParser = Parsec () String

bcgeCsvLineParser :: BcgeParser CsvLine
bcgeCsvLineParser = do
  f0 <- many (noneOf ";") <* char ';'
  f1 <- many (noneOf ";") <* char ';'
  f2 <- many (noneOf ";") <* char ';'
  many (noneOf "\r\n") <* (void eol <|> eof)
  return (f0, f1, f2)

bcgeCsvParser :: BcgeParser [CsvLine]
bcgeCsvParser = many bcgeCsvLineParser

betragParser :: BcgeParser MonetaryValue
betragParser = do
  unitsString <- some (oneOf "-0123456789")
  units :: Integer <- return $ read unitsString
  char '.'
  centsString :: String <- some $ oneOf "0123456789"
  cents :: Integer <- return $ read centsString
  return $ fromUnitsAndCents units cents

saldoParser :: BcgeParser MonetaryValue
saldoParser = do
  string "Saldo: CHF "
  betragParser

parseStatementDate :: String → Maybe Day
parseStatementDate = parseTimeM True defaultTimeLocale "%d.%m.%Y"

statementDateParser :: BcgeParser Day
statementDateParser = do
  string "Kontoauszug bis: "
  dateString <- many $ noneOf " ;"
  date <- maybe
    (fail "Could not parse the statement's day")
    return
    (parseStatementDate dateString)
  char ' '
  return date

-- Parsed Strings → Safe data (BCGE transactions)

-- | BCGE's transaction data fetched from their website.
data BcgeTransaction = BcgeTransaction{
  bTrDate     :: Day
  , bTrTitle  :: String
  , bTrAmount :: MonetaryValue
} deriving (Eq, Show)

data BcgeStatement = BcgeStatement{
  bStatementDate           :: Day
  , bStatementBalance      :: MonetaryValue
  , bStatementTransactions :: [BcgeTransaction]
} deriving (Eq, Show)

headerLine :: CsvLine
headerLine = ("Datum", "Buchungstext", "Betrag")

splitCsv :: [CsvLine] -> (Header, Content)
splitCsv csvLines = (take headerPos csvLines, drop (headerPos + 1) csvLines)
  where
    Just headerPos = elemIndex headerLine csvLines

getDate :: Header → Maybe Day
getDate header = do
  dateString <- headMay [f0 | (f0, _, _) <- header, take 11 f0 == "Kontoauszug"]
  parseMaybe statementDateParser dateString

getSaldo :: Header → Maybe MonetaryValue
getSaldo header = do
  saldoString <- headMay [f0 | (f0, _, _) <- header, take 5 f0 == "Saldo"]
  parseMaybe saldoParser saldoString

csvLineToBcgeTransaction :: CsvLine → Maybe BcgeTransaction
csvLineToBcgeTransaction (dateString, title, amountString) = do
  date <- parseTimeM True defaultTimeLocale "%d.%m.%y" dateString
  amount <- parseMaybe betragParser amountString
  return $ BcgeTransaction date title amount

csvLinesToBcgeStatement :: [CsvLine] -> BcgeStatement
csvLinesToBcgeStatement csvLines = BcgeStatement date saldo bcgeStatements
  where
    (header, content) = splitCsv csvLines
    Just date = getDate header
    Just saldo = getSaldo header
    Just bcgeStatements = traverse csvLineToBcgeTransaction content

-- Functions operating on safe data (BcgeStatement, etc.) and transforming it
-- to Ledger.

-- | TODO
--   * [ ] Sanitize titles
--   * [ ] Adjust counteraccounts
bTrToLedger :: BcgeTransaction -> Transaction
bTrToLedger (BcgeTransaction date  title  amount) =
  transaction (show date) []
  & set tDescription title
  . set tStatus Cleared

saldoToLedger :: Day -> MonetaryValue -> Transaction
saldoToLedger date balance =
  transaction (show date) [balancePosting]
  & set tDescription "BCGE Status"
  . set tStatus Cleared
  where
    balancePosting =
      nullposting
      & set pAccount bcgeAccount
      . set pBalanceAssertion (balassert . HDE.makeCurrencyAmount "CHF" $ balance)

bStToLedger :: BcgeStatement -> [Transaction]
bStToLedger (BcgeStatement date balance trs) = execWriter collectTrs
  where
  collectTrs = do
    tell $ fmap bTrToLedger (reverse trs)
    tell [saldoToLedger date balance]

bcgeCsvToLedger :: String → String
bcgeCsvToLedger input =
  let
    Right lines = parse bcgeCsvParser "" input
    bcgeStatement = csvLinesToBcgeStatement lines
    ledgerTransactions = bStToLedger bcgeStatement
  in concatMap showTransaction ledgerTransactions
