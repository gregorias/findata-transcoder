-- | A module for representing and parsing a Coop receipt.
module Transcoder.Coop.Receipt (
  Receipt (..),
  Entry (..),
  Payment (..),
  PaymentMethod (..),
  CreditCardPaymentMethod (..),
  Rabatt (..),
  receiptP,
) where

import Control.Lens ((^?))
import Control.Lens.Regex.Text (regex)
import qualified Control.Lens.Regex.Text as LR
import Data.Decimal (Decimal)
import Data.List.NonEmpty (some1)
import Data.Time (Day)
import Data.Time.Extra (dayP)
import Relude
import Relude.Extra
import Text.Megaparsec (Parsec, manyTill, manyTill_, parseMaybe, try)
import Text.Megaparsec.Char (hspace1, newline, printChar, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (..),
  DecimalFormat (..),
  DecimalFractionFormat (..),
  cashDecimalFormat,
  decimalP,
  defaultDecimalFormat,
 )

type Parser = Parsec Void Text

-- | Coop receipt.
data Receipt = Receipt
  { receiptDate :: !Day
  , receiptEntries :: ![Entry]
  , receiptRabatt :: !(Maybe Rabatt)
  , receiptTotal :: !Decimal
  , receiptPayments :: !(NonEmpty Payment)
  }

newtype Rabatt = Rabatt Decimal

rabattLineP :: Parser Rabatt
rabattLineP = try $ do
  rest :: Text <- anyLineP
  let maybeRabatt = rest ^? [regex|.* (-\d+\.\d\d)|] . LR.groups
  rabattText <-
    maybe
      (fail . toString $ "Could not parse the rabatt line: " <> rest)
      ( \case
          [rabattText] -> return rabattText
          x -> (fail $ "Could not destructure the rabatt line: " <> show x)
      )
      maybeRabatt
  total <- case parseMaybe @Void (decimalP defaultDecimalFormat) rabattText of
    Nothing -> fail . toString $ "Could not parse the rabatt's total: " <> rabattText
    Just t -> return t
  return . Rabatt $ total

instance Semigroup Rabatt where
  (<>) (Rabatt a) (Rabatt b) = Rabatt (a + b)

-- | A single purchase entry.
data Entry = Entry
  { entryName :: !Text
  , entryTotal :: !Decimal
  }

entryLineP :: Parser Entry
entryLineP = do
  entryLine <- anyLineP
  (entryName', totalText) <-
    maybe
      (fail . toString $ "Could not parse the entry line: " <> entryLine)
      return
      ( destructureMatchGroups
          =<< ( entryLine
                  ^? [regex|(.*) (\d+|\d+\.\d+) (\d+\.\d\d)( \d+\.\d\d)? (\d+\.\d\d)( \d+)?|]
                    . LR.groups
              )
      )
  total <- case parseMaybe @Void (decimalP defaultDecimalFormat) totalText of
    Nothing -> fail . toString $ "Could not parse the entry's total: " <> entryLine
    Just t -> return t
  return $ Entry entryName' total
 where
  destructureMatchGroups =
    \case
      [entryName', _menge, _preAktionTotal, _preis, total] -> Just (entryName', total)
      [entryName', _menge, _preAktionTotal, _preis, total, _zusatz] -> Just (entryName', total)
      _ -> Nothing

cashP :: Parser Decimal
cashP = decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

data Payment = Payment
  { paymentMethod :: !PaymentMethod
  , paymentTotal :: !Decimal
  }

paymentP :: Parser Payment
paymentP = do
  supercashP <|> creditCardP
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
  { -- | An obscured card number that appears in the receipt, e.g.,
    -- 'XXXXXX******1144'
    creditCardPaymentMethodObscuredCardNumber :: Text
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

maybeRabattAndTotalP :: Parser (Maybe Rabatt, Decimal)
maybeRabattAndTotalP = do
  maybeRabatt <- viaNonEmpty fold1 <$> many (rabattLineP <* many newline)
  total <- totalLineP
  return (maybeRabatt, total)

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP (dayP "%d.%m.%y" <* anyLineP)
  void $ manyTill_ anyLineP headerLineP
  (maybeEntries, (maybeRabatt, total)) <-
    manyTill_
      ((newline >> return Nothing) <|> (string "0\n" >> return Nothing) <|> (Just <$> entryLineP))
      maybeRabattAndTotalP
  void $ many newline
  Receipt day (catMaybes maybeEntries) maybeRabatt total <$> some1 (paymentP <* many newline)
