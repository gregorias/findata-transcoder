{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledupt.Bcge
  ( bcgeCsvToLedger,
    BcgeTransaction (..),
    bcgeTransactionToLedger,
    CsvLine,
    csvLineToBcgeTransaction,
    parseStatementDate,
    statementDateParser,
    saldoToLedger,
  )
where

import Control.Lens
  ( set,
    (&),
  )
import Control.Monad (void)
import Control.Monad.Writer.Lazy (execWriter, tell)
import Data.List (elemIndex)
import Data.Text (pack)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens
  ( pAccount,
    pBalanceAssertion,
    tDescription,
    tStatus,
  )
import Hledger.Data.Posting (balassert, nullposting)
import qualified Hledger.Data.Posting as Hledger
import Hledger.Data.Transaction (showTransaction, transaction)
import Hledger.Data.Types
  ( Status (..),
    Transaction (..),
  )
import qualified Hledupt.Bcge.Hint as Hint
import Hledupt.Data (MonetaryValue, decimalParser)
import Safe (headMay)
import Text.Megaparsec
  ( Parsec,
    many,
    noneOf,
    parse,
    parseMaybe,
  )
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Extra (eolOrEof)

bcgeAccount :: String
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
  void $ many (noneOf "\r\n") <* eolOrEof
  return (f0, f1, f2)

bcgeCsvParser :: BcgeParser [CsvLine]
bcgeCsvParser = many bcgeCsvLineParser

saldoParser :: BcgeParser MonetaryValue
saldoParser = do
  void $ string "Saldo: CHF "
  decimalParser

parseStatementDate :: String -> Maybe Day
parseStatementDate = parseTimeM True defaultTimeLocale "%d.%m.%Y"

statementDateParser :: BcgeParser Day
statementDateParser = do
  void $ string "Kontoauszug bis: "
  dateString <- many $ noneOf " ;"
  date <-
    maybe
      (fail "Could not parse the statement's day")
      return
      (parseStatementDate dateString)
  void $ char ' '
  return date

-- Parsed Strings → Safe data (BCGE transactions)

-- | BCGE's transaction data fetched from their website.
data BcgeTransaction = BcgeTransaction
  { bTrDate :: Day,
    bTrTitle :: String,
    bTrAmount :: MonetaryValue
  }
  deriving (Eq, Show)

data BcgeStatement = BcgeStatement
  { bStatementDate :: Day,
    bStatementBalance :: MonetaryValue,
    bStatementTransactions :: [BcgeTransaction]
  }
  deriving (Eq, Show)

headerLine :: CsvLine
headerLine = ("Datum", "Buchungstext", "Betrag")

splitCsv :: [CsvLine] -> (Header, Content)
splitCsv csvLines = (take headerPos csvLines, drop (headerPos + 1) csvLines)
  where
    Just headerPos = elemIndex headerLine csvLines

getDate :: Header -> Maybe Day
getDate header = do
  dateString <- headMay [f0 | (f0, _, _) <- header, take 11 f0 == "Kontoauszug"]
  parseMaybe statementDateParser dateString

getSaldo :: Header -> Maybe MonetaryValue
getSaldo header = do
  saldoString <- headMay [f0 | (f0, _, _) <- header, take 5 f0 == "Saldo"]
  parseMaybe saldoParser saldoString

csvLineToBcgeTransaction :: CsvLine -> Maybe BcgeTransaction
csvLineToBcgeTransaction (dateString, title, amountString) = do
  date <- parseTimeM True defaultTimeLocale "%d.%m.%y" dateString
  amount <- parseMaybe decimalParser amountString
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

bcgeTransactionToLedger :: Maybe Hint.Config -> BcgeTransaction -> Transaction
bcgeTransactionToLedger maybeConfig (BcgeTransaction date title amount) =
  transaction (show date) [bcgePosting, counterPosting]
    & set tDescription description
      . set tStatus Cleared
  where
    maybeHint = do
      config <- maybeConfig
      Hint.transactionTitleToHint config title
    description = maybe title Hint.title maybeHint
    bcgePosting =
      Hledger.post
        (pack "Assets:Liquid:BCGE")
        (HDE.makeCurrencyAmount "CHF" amount)
    counterAccount = maybe "Expenses:Other" Hint.counterAccount maybeHint
    counterPosting =
      Hledger.post
        (pack counterAccount)
        (HDE.makeCurrencyAmount "CHF" $ negate amount)

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

bStToLedger :: Maybe Hint.Config -> BcgeStatement -> [Transaction]
bStToLedger config (BcgeStatement date balance trs) = execWriter collectTrs
  where
    collectTrs = do
      tell $ fmap (bcgeTransactionToLedger config) (reverse trs)
      tell [saldoToLedger date balance]

bcgeCsvToLedger :: Maybe Hint.Config -> String -> String
bcgeCsvToLedger config input =
  let Right csvLines = parse bcgeCsvParser "" input
      bcgeStatement = csvLinesToBcgeStatement csvLines
      ledgerTransactions = bStToLedger config bcgeStatement
   in concatMap showTransaction ledgerTransactions
