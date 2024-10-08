-- | A module for representing and parsing a Coop receipt.
module Transcoder.Coop.Receipt (
  Receipt (..),
  receiptP,
  Entry (..),
  entryLineP,
  Payment (..),
  PaymentMethod (..),
  CreditCardPaymentMethod (..),
) where

import Control.Lens ((^?))
import Control.Lens.Regex.Text (regex)
import Control.Lens.Regex.Text qualified as LR
import Control.Monad (msum)
import Data.Decimal (Decimal)
import Data.Decimal.Extra (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  cashDecimalFormat,
  decimalP,
  defaultDecimalFormat,
 )
import Data.List.NonEmpty (some1)
import Data.Time (Day)
import Data.Time.Extra (dayP)
import Relude
import Text.Megaparsec (Parsec, label, manyTill, manyTill_, parseMaybe, try)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (hspace1, newline, printChar, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Error.Builder qualified as MP

type Parser = Parsec Void Text

-- | Coop receipt.
data Receipt = Receipt
  { receiptDate :: !Day
  , receiptEntries :: ![Entry]
  , receiptTotal :: !Decimal
  , receiptPayments :: !(NonEmpty Payment)
  }

receiptP :: Parser Receipt
receiptP = do
  day <- ignoreLinesTill dayLineP
  void $ ignoreLinesTill headerLineP
  entries <- many entryP
  -- Ignore entries with total 0, because they are not interesting for my accounting.
  let interestingEntries = filter (\e -> entryTotal e /= 0) entries
  total <- totalP
  void $ many newline
  Receipt day interestingEntries total <$> some1 (paymentP <* many newline)
 where
  ignoreLinesTill :: Parser a -> Parser a
  ignoreLinesTill p = do
    (_, a) <- manyTill_ anyLineP p
    return a
  dayLineP :: Parser Day
  dayLineP = dayP "%d.%m.%y" <* anyLineP
  entryP :: Parser Entry
  entryP = label "entry lines" . try $ do
    void $ many (void newline <|> void (string "0\n"))
    entryLineP
  totalP = label "total lines (\"Total CHF\" preceded by newlines)" . try $ do
    void $ many (void newline <|> void (string "0\n"))
    totalLineP

-- | A single purchase entry.
data Entry = Entry
  { entryName :: !Text
  , entryTotal :: !Decimal
  }
  deriving stock (Eq, Show)

parseEntryLineAsDeduction :: Text -> Maybe Entry
parseEntryLineAsDeduction line = do
  parsedLine <- line ^? [regex|(.*) (-\d+\.\d\d)\n|] . LR.groups
  (name, totalText) <- case parsedLine of
    [deductionName, deductionTotal] -> return (deductionName, deductionTotal)
    _ -> Nothing
  total <- parseMaybe @Void (decimalP defaultDecimalFormat) totalText
  return $ Entry name total

parseEntryLineAsRegular :: Text -> Maybe Entry
parseEntryLineAsRegular entryLine = do
  (entryName', totalText) <-
    destructureMatchGroups
      =<< ( entryLine
              ^? [regex|(.*) (-?\d+|-?\d+\.\d+) (\d+\.\d\d)( \d+\.\d\d)? (-?\d+\.\d\d)( \d+)?|] . LR.groups
          )
  total <- parseMaybe @Void (decimalP defaultDecimalFormat) totalText
  return $ Entry entryName' total
 where
  destructureMatchGroups =
    \case
      [entryName', _menge, _preAktionTotal, _preis, total] -> Just (entryName', total)
      [entryName', _menge, _preAktionTotal, _preis, total, _zusatz] -> Just (entryName', total)
      _ -> Nothing

parseEntryLine :: Text -> Maybe Entry
parseEntryLine line = msum $ [parseEntryLineAsRegular, parseEntryLineAsDeduction] <*> pure line

entryLineP :: Parser Entry
entryLineP = (label "entry line" . MP.try) $ do
  -- The total line looks like an entry line, but is not. Don’t accept it.
  MP.notFollowedBy (string "Total CHF ")
  offset <- MP.getOffset
  entryLine <- anyLineP
  maybe
    (MP.parseError $ MP.err offset (MP.utoks entryLine))
    return
    (parseEntryLine entryLine)

cashP :: Parser Decimal
cashP = decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

data Payment = Payment
  { paymentMethod :: !PaymentMethod
  , paymentTotal :: !Decimal
  }

paymentP :: Parser Payment
paymentP = do
  supercashP
    <|> creditCardP
    <|> ( do
            method <- (string "TWINT" >> return TWINT) <|> (string "Superpunkte" >> return Superpunkte)
            total <- hspace1 >> cashP
            return $ Payment method total
        )
 where
  supercashP :: Parser Payment
  supercashP = do
    Payment Supercash <$> (string "Supercash" >> hspace1 >> decimalP defaultDecimalFormat >> hspace1 >> decimalP (cashDecimalFormat NoChunkSep) >> hspace1 >> cashP)

  creditCardP :: Parser Payment
  creditCardP = do
    total <- ((string "Mastercard" <|> string "VISA") >> hspace1 >> cashP) <* newline
    void $ many newline
    void $ string "Buchung" >> anyLineP
    void $ many newline
    cardnumber <- toText <$> manyTill printChar newline
    return $ Payment (CreditCard $ CreditCardPaymentMethod cardnumber) total

newtype CreditCardPaymentMethod = CreditCardPaymentMethod
  { creditCardPaymentMethodObscuredCardNumber :: Text
  -- ^ An obscured card number that appears in the receipt, e.g.,
  -- 'XXXXXX******1144'
  }

data PaymentMethod
  = TWINT
  | CreditCard CreditCardPaymentMethod
  | Supercash
  | Superpunkte

headerLineP :: Parser ()
headerLineP = void $ string "Artikel Menge Preis Aktion Total Zusatz\n"

totalLineP :: Parser Decimal
totalLineP = do
  void $ string "Total CHF "
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
