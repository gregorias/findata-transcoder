{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledupt.Bcge (
  bcgeCsvToLedger,
  BcgeTransaction (..),
  bcgeTransactionToLedger,
  CsvLine,
  csvLineToBcgeTransaction,
  parseStatementDate,
  statementDateParser,
  saldoToLedger,
) where

import Control.Lens (
  set,
 )
import qualified Control.Lens as L
import Control.Monad.Writer.Lazy (execWriter, tell)
import Data.Decimal (Decimal)
import Data.List (elemIndex)
import Data.Text (pack)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (
  pAccount,
  pBalanceAssertion,
  tDescription,
  tStatus,
 )
import Hledger.Data.Posting (balassert, nullposting)
import qualified Hledger.Data.Posting as Hledger
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types (
  Status (..),
  Transaction (..),
 )
import qualified Hledupt.Bcge.Hint as Hint
import Hledupt.Data.Cash (Cash (Cash), cashAmount)
import Hledupt.Data.Currency (Currency (CHF))
import Hledupt.Data.LedgerReport (LedgerReport (LedgerReport))
import Hledupt.Data.MyDecimal (decimalP)
import Relude
import Safe (headMay)
import Text.Megaparsec (
  Parsec,
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

saldoParser :: BcgeParser Cash
saldoParser = do
  void $ string "Saldo: CHF "
  Cash CHF <$> decimalP

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
  { bTrDate :: Day
  , bTrTitle :: String
  , bTrAmount :: Decimal
  }
  deriving stock (Eq, Show)

data BcgeStatement = BcgeStatement
  { bStatementDate :: Day
  , bStatementBalance :: Decimal
  , bStatementTransactions :: [BcgeTransaction]
  }
  deriving stock (Eq, Show)

headerLine :: CsvLine
headerLine = ("Datum", "Buchungstext", "Betrag")

splitCsv :: [CsvLine] -> Maybe (Header, Content)
splitCsv csvLines = do
  headerPos <- elemIndex headerLine csvLines
  return (take headerPos csvLines, drop (headerPos + 1) csvLines)

getDate :: Header -> Maybe Day
getDate header = do
  dateString <- headMay [f0 | (f0, _, _) <- header, take 11 f0 == "Kontoauszug"]
  parseMaybe statementDateParser dateString

getSaldo :: Header -> Maybe Cash
getSaldo header = do
  saldoString <- headMay [f0 | (f0, _, _) <- header, take 5 f0 == "Saldo"]
  parseMaybe saldoParser saldoString

csvLineToBcgeTransaction :: CsvLine -> Maybe BcgeTransaction
csvLineToBcgeTransaction (dateString, title, amountString) = do
  date <- parseTimeM True defaultTimeLocale "%d.%m.%y" dateString
  amount <- parseMaybe decimalP amountString
  return $ BcgeTransaction date title amount

csvLinesToBcgeStatement :: [CsvLine] -> Maybe BcgeStatement
csvLinesToBcgeStatement csvLines = do
  (header, content) <- splitCsv csvLines
  date <- getDate header
  saldo <- getSaldo header
  bcgeStatements <- traverse csvLineToBcgeTransaction content
  return $ BcgeStatement date (L.view cashAmount saldo) bcgeStatements

-- Functions operating on safe data (BcgeStatement, etc.) and transforming it
-- to Ledger.

bcgeTransactionToLedger :: Maybe Hint.Config -> BcgeTransaction -> Transaction
bcgeTransactionToLedger maybeConfig (BcgeTransaction date title amount) =
  transaction date [bcgePosting, counterPosting]
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
      (HDE.makeCurrencyAmount CHF amount)
  counterAccount = maybe "Expenses:Other" Hint.counterAccount maybeHint
  counterPosting =
    Hledger.post
      (pack counterAccount)
      (HDE.makeCurrencyAmount CHF $ negate amount)

saldoToLedger :: Day -> Decimal -> Transaction
saldoToLedger date balance =
  transaction date [balancePosting]
    & set tDescription "BCGE Status"
      . set tStatus Cleared
 where
  balancePosting =
    nullposting
      & set pAccount bcgeAccount
        . set pBalanceAssertion (balassert . HDE.makeCurrencyAmount CHF $ balance)

bStToLedger :: Maybe Hint.Config -> BcgeStatement -> [Transaction]
bStToLedger config (BcgeStatement date balance trs) = execWriter collectTrs
 where
  collectTrs = do
    tell $ fmap (bcgeTransactionToLedger config) (reverse trs)
    tell [saldoToLedger date balance]

bcgeCsvToLedger :: Maybe Hint.Config -> String -> Either Text LedgerReport
bcgeCsvToLedger config input =
  case ledgerTrs of
    Left err -> Left . Text.pack $ show err
    Right Nothing -> Left "Something went wrong in the transformation process."
    Right (Just res) -> Right $ LedgerReport res []
 where
  csvLines = parse bcgeCsvParser "" input
  bcgeStatement :: Either _ (Maybe BcgeStatement) =
    fmap csvLinesToBcgeStatement csvLines
  ledgerTrs = fmap (fmap (bStToLedger config)) bcgeStatement
