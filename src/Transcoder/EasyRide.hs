module Transcoder.EasyRide (
  receiptToLedger,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Time (Day, fromGregorian)
import Data.Time.Calendar.Extra (monthP)
import Hledger (
  Status (Cleared, Pending),
  Transaction,
  transaction,
 )
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Relude
import Text.Megaparsec (
  Parsec,
  anySingle,
  label,
  manyTill_,
 )
import Text.Megaparsec.Char (
  char,
  string,
 )
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (ChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Transcoder.Wallet (bcgeCCAccount, expensesTransport)

data Receipt = Receipt
  { _receiptDate :: !Day
  , _receiptBetrag :: !Decimal
  }

type Parser = Parsec Void Text

dateLineP :: Parser Day
dateLineP = label "date line" $ do
  void $ string "Zahlungsbeleg – " <|> string "Rechnungsdatum: " <|> "Invoicing date: "
  day <- decimal
  void $ char ' '
  month <- monthP
  void $ char ' '
  year <- decimal
  void anyLineP
  return $ fromGregorian year month day

totalLineP :: Parser Decimal
totalLineP = label "total bill line" $ do
  void $ string "Verrechneter Gesamtbetrag" <|> string "Total amount charged"
  void $ manyTill_ anySingle (string "CHF ")
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  (_, total) <- manyTill_ anyLineP totalLineP
  void anyLineP
  return $ Receipt day total

receiptToTransaction :: Receipt -> Transaction
receiptToTransaction (Receipt day total) =
  transaction day postings
    & L.set tDescription "EasyRide"
      . L.set tStatus Cleared
 where
  postings = [bcgePosting, transportPosting]
  bcgePosting =
    Ledger.post
      bcgeCCAccount
      (HDE.makeCurrencyAmount chf (-total))
      & L.set pStatus Pending
  transportPosting =
    Ledger.post
      expensesTransport
      (HDE.makeCurrencyAmount chf total)

receiptToLedger :: Text -> Either Text Transaction
receiptToLedger receiptText = do
  receipt <- parsePretty receiptP "EasyRide receipt" receiptText
  return $ receiptToTransaction receipt
