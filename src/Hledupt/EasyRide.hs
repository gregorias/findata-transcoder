module Hledupt.EasyRide (
  receiptToLedger,
) where

import qualified Control.Lens as L
import Data.Decimal (Decimal)
import Data.Time (Day, fromGregorian)
import Data.Time.Calendar.MonthDay.Compat (MonthOfYear)
import Hledger (
  Status (Cleared, Pending),
  Transaction,
  transaction,
 )
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Hledupt.Wallet (bcgeCCAccount, expensesTransport)
import Relude
import Text.Megaparsec (
  Parsec,
  anySingle,
  errorBundlePretty,
  manyTill,
  manyTill_,
  parse,
 )
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Receipt = Receipt
  { _receiptDate :: !Day
  , _receiptBetrag :: !Decimal
  }

type Parser = Parsec Void Text

anyLineP :: Parsec Void Text Text
anyLineP = toText <$> manyTill anySingle newline

germanMonthToMonthOfYear :: Text -> Maybe MonthOfYear
germanMonthToMonthOfYear "Januar" = return 1
germanMonthToMonthOfYear "Februar" = return 2
germanMonthToMonthOfYear "März" = return 3
germanMonthToMonthOfYear "April" = return 4
germanMonthToMonthOfYear "Mai" = return 5
germanMonthToMonthOfYear "Juni" = return 6
germanMonthToMonthOfYear "Juli" = return 7
germanMonthToMonthOfYear "August" = return 8
germanMonthToMonthOfYear "Aug." = return 8
germanMonthToMonthOfYear "September" = return 9
germanMonthToMonthOfYear "Oktober" = return 10
germanMonthToMonthOfYear "November" = return 11
germanMonthToMonthOfYear "Dezember" = return 12
germanMonthToMonthOfYear _ = Nothing

dateLineP :: Parser Day
dateLineP = do
  void $ string "Zahlungsbeleg – "
  day <- decimal
  void $ char ' '
  monthString <- toText <$> some (letterChar <|> char '.')
  void $ char ' '
  year <- decimal
  void anyLineP
  month <-
    maybe
      (fail $ "Could not parse the expected month string: " <> toString monthString)
      return
      (germanMonthToMonthOfYear monthString)
  return $ fromGregorian year month day

totalLineP :: Parser Decimal
totalLineP = do
  void $ string "Verrechneter Gesamtbetrag CHF "
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  (_, total) <- manyTill_ anyLineP totalLineP
  void anyLineP
  return $ Receipt day total

prependErrorMessage :: Text -> Either Text a -> Either Text a
prependErrorMessage err = L._Left L.%~ ((err <> "\n") <>)

parseReceipt :: Text -> Either Text Receipt
parseReceipt receipt = prepareErrMsg parsedReceipt
 where
  parsedReceipt = parse receiptP "" receipt
  prepareErrMsg =
    prependErrorMessage "Could not parse the receipt."
      . first (toText . errorBundlePretty)

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
      (HDE.makeCurrencyAmount chf (- total))
      & L.set pStatus Pending
  transportPosting =
    Ledger.post
      expensesTransport
      (HDE.makeCurrencyAmount chf total)

receiptToLedger :: Text -> Either Text Transaction
receiptToLedger receiptText = do
  receipt <- parseReceipt receiptText
  return $ receiptToTransaction receipt
