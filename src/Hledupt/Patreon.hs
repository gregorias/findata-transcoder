module Hledupt.Patreon (
  receiptToLedger,
) where

import Control.Lens (
  set,
 )
import Data.Char (isDigit)
import Data.Decimal (Decimal)
import qualified Data.Text as Text
import Data.Time.Calendar (
  Day,
  fromGregorianValid,
 )
import Data.Time.Calendar.Extra (
  monthP,
 )
import Hledger (
  Posting,
  Status (Cleared, Pending),
  Transaction,
  post,
  transaction,
 )
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Lens (
  pComment,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledupt.Data.Currency (
  usd,
 )
import Hledupt.Data.MyDecimal (
  decimalP,
  defaultDecimalFormat,
 )
import Hledupt.Wallet (
  expenses,
  revolutAccount,
  (<:>),
 )
import Relude
import Relude.Unsafe (fromJust)
import Text.Megaparsec (
  Parsec,
  anySingle,
  manyTill,
  single,
  takeWhile1P,
  takeWhileP,
  try,
 )
import Text.Megaparsec.Char (
  space,
  string,
 )
import Text.Megaparsec.Char.Lexer (
  decimal,
 )
import Text.Megaparsec.Extra (
  parsePretty,
 )

data Entry = Entry
  { name :: !Text
  , total :: !Decimal
  }

data Receipt = Receipt
  { date :: !Day
  , entries :: [Entry]
  , total :: !Decimal
  }

type Parser = Parsec Void Text

dateP :: Parser Day
dateP = do
  month <- monthP
  space
  day <- decimal
  single ',' >> space
  year <- decimal
  return . fromJust $ fromGregorianValid year month day

urlP :: Parser Text
urlP = do
  void $ single '['
  takeWhile1P (Just "url") (/= ']') <* single ']'

entryP :: Parser Entry
entryP = do
  try (space >> urlP) >> space
  pupil <- Text.strip <$> takeWhile1P (Just "pupil") (/= '[')
  void $ space >> urlP >> space >> string "=20\r\n$"
  total' <- decimalP defaultDecimalFormat
  void $ manyTill anySingle (single '$' >> takeWhileP Nothing (\c -> isDigit c || c == '.'))
  return $ Entry{name = pupil, total = total'}

receiptP :: Parser Receipt
receiptP = do
  void $ manyTill anySingle (string "Charge date: ")
  day <- dateP
  void $ manyTill anySingle (string "Total $")
  total' <- decimalP defaultDecimalFormat
  void $ manyTill anySingle (string "Charge details")
  entries' <- some entryP
  return $ Receipt day entries' total'

entryToPosting :: Entry -> Posting
entryToPosting Entry{name = name', total = total'} =
  post (expenses <:> "Leisure:Patreon") (makeCurrencyAmount usd total')
    & set pComment name'

receiptToTransaction :: Receipt -> Transaction
receiptToTransaction Receipt{date = date', entries = entries', total = total'} =
  transaction date' postings
    & set tDescription "Patreon"
      . set tStatus Cleared
 where
  revolutPosting =
    post (revolutAccount <:> "USD") (makeCurrencyAmount usd (- total'))
      & set pStatus Pending
  postings = [revolutPosting] <> (entryToPosting <$> entries')

receiptToLedger :: Text -> Either Text Transaction
receiptToLedger receiptText = do
  receipt <- parsePretty receiptP "the Patreon receipt" receiptText
  return $ receiptToTransaction receipt
