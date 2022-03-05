module Hledupt.Galaxus (
  parseReceipt,
) where

import qualified Data.Text as T
import Data.Time (
  Day,
  defaultTimeLocale,
  parseTimeM,
 )
import Hledger (AccountName, Status (Cleared, Pending), Transaction)
import Hledger.Data.Extra (
  Comment (..),
  ToPosting (..),
  ToTransaction (..),
  makePosting,
  makeTransaction,
 )
import Hledupt.Data.Cash (Cash (Cash))
import qualified Hledupt.Data.Cash as Cash
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (..),
  cashDecimalFormat,
  decimalP,
 )
import Hledupt.Wallet (bcgeAccount, bcgeCCAccount)
import Relude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Extra as MP
import qualified Text.Megaparsec.Char.Lexer as MP
import qualified Text.Megaparsec.Extra as MP

data Item = Item
  { _count :: !Natural
  , _description :: !Text
  , _itemCost :: !Cash
  }

instance ToPosting Item where
  toPosting (Item _ description cost) =
    makePosting (Just Pending) "Todo" (Just cost) (Comment description)

data Receipt = Receipt
  { _date :: !Day
  , _items :: !(NonEmpty Item)
  , _paymentSource :: !AccountName
  , _total :: !Cash
  }

instance ToTransaction Receipt where
  toTransaction (Receipt date items paymentSource total) =
    makeTransaction
      date
      (Just Cleared)
      "Digitec/Galaxus"
      ( [paymentPosting] <> toList (toPosting <$> items)
      )
   where
    paymentPosting = makePosting (Just Pending) paymentSource (Just total) NoComment

type Parser = MP.Parsec Void Text

galaxusCashP :: Parser Cash
galaxusCashP = Cash chf <$> (noRappenP <|> rappenP)
 where
  noRappenP = MP.try $ MP.decimal <* MP.string ".–"
  rappenP = MP.try $ decimalP (cashDecimalFormat NoChunkSep)

itemP :: Parser Item
itemP = do
  count <- fromInteger <$> (MP.decimal <* MP.single 'x')
  (description, itemCost) <- MP.manyTill_ MP.anySingle galaxusCashP <* MP.eol
  void $ MP.manyTill MP.anyLineP MP.eol
  return $ Item count (T.strip $ toText description) itemCost

dateP :: Parser Day
dateP = do
  dateText <- MP.anyLineP
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000 (UTC)" (toString dateText)

gesamtbetragP :: Parser Cash
gesamtbetragP = MP.string "Gesamtbetrag: " >> galaxusCashP <* MP.eol

zahlungsmittelP :: Parser AccountName
zahlungsmittelP = do
  void $ MP.string "Zahlungsmittel" >> MP.eol
  (MP.string "TWINT" >> return bcgeAccount)
    <|> (MP.string "MasterCard" >> return bcgeCCAccount)

receiptP :: Parser Receipt
receiptP = do
  date <- dateP
  (_, firstItem) <- MP.manyTill_ MP.anyLineP itemP
  otherItems <- many itemP
  total <- gesamtbetragP
  (_, paymentSource) <- MP.manyTill_ MP.anyLineP zahlungsmittelP
  return $
    Receipt
      { _date = date
      , _items = firstItem :| otherItems
      , _paymentSource = paymentSource
      , _total = Cash.negate total
      }

parseReceipt :: Text -> Either Text Transaction
parseReceipt receiptTxt = do
  receipt <- MP.parsePretty receiptP "a Digitec/Galaxus receipt" receiptTxt
  return $ toTransaction receipt
