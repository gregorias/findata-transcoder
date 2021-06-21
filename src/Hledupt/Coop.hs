module Hledupt.Coop (
  receiptToLedger,
) where

import Control.Lens (
  has,
  (^?),
 )
import qualified Control.Lens as L
import Control.Lens.Regex.Text (regex)
import qualified Control.Lens.Regex.Text as LR
import Data.Decimal (Decimal)
import qualified Data.HashMap.Strict as HashMap
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (Status (Cleared, Pending), Transaction, transaction)
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
  defaultDecimalFormat,
 )
import Relude
import Relude.Extra (fold1, groupBy)
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  anySingle,
  count,
  errorBundlePretty,
  manyTill,
  manyTill_,
  match,
  parse,
  parseMaybe,
 )
import Text.Megaparsec.Char (char, digitChar, newline, string)

data Receipt = Receipt
  { _receiptDate :: !Day
  , _receiptEntries :: [Entry]
  , _receiptRabatt :: !(Maybe Rabatt)
  , _receiptTotal :: !Decimal
  , _receiptPaymentMethod :: !PaymentMethod
  }

data Entry = Entry
  { _entryName :: !Text
  , _entryTotal :: !Decimal
  }

data PaymentMethod = TWINT | Mastercard

paymentMethodToAccount :: PaymentMethod -> Text
paymentMethodToAccount TWINT = "Assets:Liquid:BCGE"
paymentMethodToAccount Mastercard = "Assets:Liquid:BCGE CC"

type Parser = Parsec Void Text

anyLineP :: Parsec Void Text Text
anyLineP = toText <$> manyTill anySingle newline

dateLineP :: Parser Day
dateLineP = do
  (dateString, _) <-
    try $
      match
        (count 2 digitChar >> char '.' >> count 2 digitChar >> char '.' >> many digitChar)
  void anyLineP
  parseTimeM False defaultTimeLocale "%d.%m.%y" (toString dateString)

headerLineP :: Parser ()
headerLineP = void $ string "Artikel Menge Preis Aktion Total Zusatz\n"

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
    Nothing -> fail . toString $ "Could not parse the entrie's total: " <> entryLine
    Just t -> return t
  return $ Entry entryName' total
 where
  destructureMatchGroups =
    \case
      [entryName', _menge, _preAktionTotal, _preis, total] -> Just (entryName', total)
      [entryName', _menge, _preAktionTotal, _preis, total, _zusatz] -> Just (entryName', total)
      _ -> Nothing

newtype Rabatt = Rabatt Decimal

instance Semigroup Rabatt where
  (<>) (Rabatt a) (Rabatt b) = Rabatt (a + b)

maybeRabattAndTotalP :: Parser (Maybe Rabatt, Decimal)
maybeRabattAndTotalP = do
  maybeRabatt <- viaNonEmpty fold1 <$> many (rabattLineP <* many newline)
  total <- totalLineP
  return (maybeRabatt, total)

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

totalLineP :: Parser Decimal
totalLineP = do
  void $ string "Total CHF "
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

paymentMethodP :: Parser PaymentMethod
paymentMethodP =
  (string "TWINT" >> anyLineP >> return TWINT)
    <|> (string "Mastercard" >> anyLineP >> return Mastercard)

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  void $ manyTill anyLineP headerLineP
  (maybeEntries, (maybeRabatt, total)) <-
    manyTill_
      ((newline >> return Nothing) <|> (Just <$> entryLineP))
      maybeRabattAndTotalP
  void $ many newline
  Receipt day (catMaybes maybeEntries) maybeRabatt total <$> paymentMethodP

entryNameToExpenseCategory :: Text -> Text
entryNameToExpenseCategory entry =
  fromMaybe
    "Groceries"
    ( listToMaybe (mapMaybe (\(rgx, cat) -> if has rgx entry then Just cat else Nothing) itemToExpenseCategoryPairs)
    )
 where
  haushalt = "Haushalt"
  readyMeals = "Groceries:Ready Meals"
  health = "Gesundheit"
  itemToExpenseCategoryPairs =
    [ ([regex|Stimorol|], "Groceries:Chewing Gum")
    , ([regex|Salmon Poké|], readyMeals)
    , ([regex|ZENBU|], readyMeals)
    , ([regex|Findus Egli|], readyMeals)
    , ([regex|[tT]eelicht|], haushalt)
    , ([regex|WC Frisch|], haushalt)
    , ([regex|Haushalt|], haushalt)
    , ([regex|Schwamm|], haushalt)
    , ([regex|Desinfektionstücher|], haushalt)
    , ([regex|^Persil|], haushalt)
    , ([regex|^Flup|], haushalt)
    , ([regex|^Sun|], haushalt)
    , ([regex|Reinigung|], haushalt)
    , ([regex|Compo-Bag|], haushalt)
    , ([regex|Brita|], haushalt)
    , ([regex|ZEISS|], haushalt)
    , ([regex|Finish|], haushalt)
    , ([regex|Listerine|], health)
    , ([regex|Nivea|], health)
    ]

prependErrorMessage :: Text -> Either Text a -> Either Text a
prependErrorMessage err = L._Left L.%~ ((err <> "\n") <>)

parseReceipt :: Text -> Either Text Receipt
parseReceipt receipt = prepareErrMsg parsedReceipt
 where
  parsedReceipt = parse receiptP "" receipt
  prepareErrMsg =
    prependErrorMessage "Could not parse the payslip."
      . first (toText . errorBundlePretty)

receiptToTransaction :: Receipt -> Transaction
receiptToTransaction (Receipt day entries rabatt total paymentMethod) =
  transaction day postings
    & L.set tDescription "Coop"
      . L.set tStatus Cleared
 where
  postings = [bcgePosting] <> items <> rabattPosting
  bcgePosting =
    Ledger.post
      (paymentMethodToAccount paymentMethod)
      (HDE.makeCurrencyAmount chf (- total))
      & L.set pStatus Pending
  (catAndTotals :: [(Text, Decimal)]) =
    map
      (\(Entry name total') -> ("Expenses:" <> entryNameToExpenseCategory name, total'))
      entries
  catToTotals :: HashMap Text (NonEmpty Decimal) = snd <<$>> groupBy fst catAndTotals
  catToTotal = sum <$> catToTotals
  items =
    map
      ( \(cat, total') ->
          Ledger.post
            cat
            (HDE.makeCurrencyAmount chf total')
      )
      (HashMap.toList catToTotal)
  rabattToPosting (Rabatt rabattVal) =
    Ledger.post
      "Expenses:Other"
      (HDE.makeCurrencyAmount chf rabattVal)
  rabattPosting = rabattToPosting <$> maybeToList rabatt

receiptToLedger :: Text -> Either Text Transaction
receiptToLedger receiptText = do
  receipt <- parseReceipt receiptText
  return $ receiptToTransaction receipt
