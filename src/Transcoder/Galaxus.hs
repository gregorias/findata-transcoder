module Transcoder.Galaxus (
  parseReceipt,
) where

import Control.Applicative.Combinators.Extra (some1Till_)
import Data.Cash (Cash (Cash))
import Data.Cash qualified as Cash
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  cashDecimalFormat,
  decimalP,
 )
import Data.Text qualified as T
import Data.Time (
  Day,
  defaultTimeLocale,
  parseTimeM,
 )
import Hledger (AccountName, Posting, Status (Cleared, Pending), Transaction)
import Hledger.Data.Extra (
  Comment (..),
  ToAmount (toAmount),
  ToTransaction (..),
  makePosting,
  makeTransaction,
 )
import Numeric.PositiveNatural (PositiveNatural, positiveNaturalP)
import Numeric.PositiveNatural qualified as PN
import Relude
import Text.Megaparsec (single)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Char.Extra qualified as MP
import Text.Megaparsec.Char.Lexer qualified as MP
import Text.Megaparsec.Extra (parsePretty)
import Transcoder.Data.Currency (chf)
import Transcoder.Wallet (bcgeAccount, bcgeCCAccount)

data Item = Item
  { itemCount :: !PositiveNatural
  , itemDescription :: !Text
  , itemCost :: !Cash
  }

data ItemType = Co2Compensation | UnknownItemType

detectItemType :: Text -> ItemType
detectItemType "CO2-Kompensation" = Co2Compensation
detectItemType _ = UnknownItemType

itemToPosting :: Item -> Posting
itemToPosting item =
  let optionalItemCount = if itemCount item == PN.one then "" else " (" <> show (itemCount item) <> "x)"
   in makePosting
        status
        postingCategory
        (Just . toAmount $ itemCost item)
        (Comment $ itemDescription item <> optionalItemCount)
 where
  itemType = detectItemType $ itemDescription item
  status = case itemType of
    Co2Compensation -> Nothing
    UnknownItemType -> Just Pending
  postingCategory = case itemType of
    Co2Compensation -> "Expenses:Other:CO2"
    UnknownItemType -> "Expenses:Todo"

data Receipt = Receipt
  { receiptDate :: !Day
  , receiptItems :: !(NonEmpty Item)
  , receiptPaymentSource :: !AccountName
  , receiptTotal :: !Cash -- The total amount of the receipt (a positive number).
  }

instance ToTransaction Receipt where
  toTransaction (Receipt date items paymentSource total) =
    makeTransaction
      date
      (Just Cleared)
      "Digitec/Galaxus"
      ([paymentPosting] <> toList (itemToPosting <$> items))
   where
    paymentPosting = makePosting Pending paymentSource (toAmount $ Cash.negate total) NoComment

type Parser = MP.Parsec Void Text

galaxusCashP :: Parser Cash
galaxusCashP = Cash chf <$> MP.try ((noRappenP <|> rappenP) <* itemEnd)
 where
  noRappenP = MP.try $ MP.decimal <* MP.string ".–"
  rappenP = MP.try $ decimalP (cashDecimalFormat NoChunkSep)
  itemEnd = MP.many MP.separatorChar >> MP.eol

itemP :: Parser Item
itemP = do
  count <- positiveNaturalP <* single '×'
  (description, cost) <- MP.manyTill_ MP.anySingle galaxusCashP
  return $ Item count (T.strip . replaceNewline $ toText description) cost
 where
  replaceNewline = T.replace "\n" " "

dateP :: Parser Day
dateP = do
  dateText <- MP.anyLineP
  parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000 (UTC)" (toString dateText)

gesamtbetragP :: Parser Cash
gesamtbetragP = string "Gesamtbetrag" >> newline >> galaxusCashP

zahlungsmittelP :: Parser AccountName
zahlungsmittelP = do
  void $ string "Zahlungsmittel:"
  (string "TWINT" >> return bcgeAccount)
    <|> (string "MasterCard" >> return bcgeCCAccount)
    <|> (string "PayPal" >> return bcgeCCAccount)

receiptP :: Parser Receipt
receiptP = do
  date <- dateP
  (items, total) <- some1Till_ itemP gesamtbetragP
  void newline
  paymentSource <- zahlungsmittelP
  return
    $ Receipt
      { receiptDate = date
      , receiptItems = items
      , receiptPaymentSource = paymentSource
      , receiptTotal = total
      }

parseReceipt :: Text -> Either Text Transaction
parseReceipt receiptTxt = do
  receipt <- parsePretty receiptP "Digitec/Galaxus receipt" receiptTxt
  return $ toTransaction receipt
