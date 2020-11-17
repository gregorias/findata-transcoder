{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Mbank (
  MbankTransaction(..),
  valueParser,
  mbankCsvParser,
  mTrToLedger,
  mbankCsvToLedger, pln) where

import           Control.Lens             (makeLenses, over, (.~), (^.))
import           Control.Monad            (void)
import           Data                     (MonetaryValue, fromUnitsAndCents)
import           Data.Decimal             (Decimal, realFracToDecimal)
import           Data.List                (sortOn)

import           Data.Time.Calendar       (Day)
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)

import           Text.Megaparsec          (Parsec, eof, many, noneOf, oneOf,
                                           parse, (<|>))
import           Text.Megaparsec.Char     (char, newline, string)

import           Data.Text                (pack)

import           Hledger.Data.Amount      (amountWithCommodity, num)
import           Hledger.Data.Posting     (balassert, nullposting, post')
import           Hledger.Data.Transaction (showTransaction, transaction)
import           Hledger.Data.Types       (Amount (..), AmountStyle (..),
                                           Posting (..), Quantity (..),
                                           Side (..), Transaction (..))

import           Hledger.Data.Lens        (aStyle, asCommoditySide,
                                           asCommoditySpaced, asPrecision)

setter :: Amount -> Amount
setter = over aStyle $ (asPrecision .~ 2) . (asCommoditySide .~ R) . (asCommoditySpaced .~ True)

pln :: Quantity -> Amount
pln d = setter rawAmt
  where
    rawAmt = amountWithCommodity (pack "PLN") (num d)


-- | mBank's transaction data fetched from their website.
data MbankTransaction = MbankTransaction{mTrDate       :: Day,
                                         mTrTitle      :: String,
                                         mTrAmount     :: MonetaryValue,
                                         mTrEndBalance :: MonetaryValue}
                                         deriving (Eq, Show)

-- Parser functionality (CSV String → [MbankTransaction])

type MbankParser = Parsec () String

headerParser :: MbankParser ()
headerParser = void (string "#Data operacji;#Opis operacji;#Rachunek;#Kategoria;#Kwota;#Saldo po operacji;\n")

dateParser :: MbankParser Day
dateParser = do
  dateString <- many (oneOf "-0123456789")
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

valueParser :: MbankParser Decimal
valueParser = do
  zloteString <- removeSpaces <$> many (oneOf "-0123456789 ")
  zlote :: Integer <- return $ read zloteString
  groszeString <- (char ',' *> many (oneOf "0123456789") <* char ' ') <|> pure "00"
  grosze :: Integer <- return $ read groszeString
  string "PLN"
  return $ fromUnitsAndCents zlote grosze
  where
    removeSpaces = filter ((/=) ' ')

mbankCsvTransactionParser :: MbankParser MbankTransaction
mbankCsvTransactionParser = do
  date <- dateParser <* char ';'
  title <- char '"' *> many (noneOf "\";") <* char '"' <* char ';'
  many (noneOf ";") >> char ';'
  many (noneOf ";") >> char ';'
  value <- valueParser <* char ';'
  balance <- valueParser <* char ';'
  void newline <|> eof
  return $ MbankTransaction date title value balance

mbankCsvParser :: MbankParser [MbankTransaction]
mbankCsvParser = do
  headerParser
  many mbankCsvTransactionParser <* eof

-- MbankTransaction to Ledger transformers

sanitizeTitle :: String → String
sanitizeTitle = beforeTheGap
    where
      beforeTheGap s | take 3 s == "   " = ""
                     | s == ""             = ""
                     | otherwise           = head s : beforeTheGap (tail s)


mTrToLedger :: MbankTransaction -> Transaction
mTrToLedger mTr = tr{tdescription=pack $ sanitizeTitle $ mTrTitle mTr}
  where
    tr = transaction
      (show . mTrDate $ mTr)
      [post' (pack "Assets:Liquid:mBank") (pln $ mTrAmount mTr) (balassert $ pln $ mTrEndBalance mTr),
       nullposting{paccount=pack "Expenses:Other"}]

--

mbankCsvToLedger :: String → String
mbankCsvToLedger inputCsv =
  let
    Right mtransactions = parse mbankCsvParser "" inputCsv
    sortedMTransactions = sortOn mTrDate mtransactions
    ltransactions = fmap mTrToLedger sortedMTransactions
  in concatMap showTransaction ltransactions
