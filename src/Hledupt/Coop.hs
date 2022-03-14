{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
import Data.Decimal (Decimal, realFracToDecimal)
import qualified Data.Decimal as D
import qualified Data.HashMap.Strict as HashMap
import Data.List.NonEmpty (some1)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (AccountName, Posting, Status (Cleared, Pending, Unmarked), Transaction, transaction)
import qualified Hledger as Ledger
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pStatus, tDescription, tStatus)
import Hledupt.Coop.Config (Config (..), getDebtors)
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  cashDecimalFormat,
  decimalP,
  defaultDecimalFormat,
 )
import Hledupt.Wallet (bcgeAccount, bcgeCCAccount, expenses, (<:>))
import qualified Hledupt.Wallet as Wallet
import Relude
import Relude.Extra (fold1, groupBy)
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  count,
  manyTill,
  manyTill_,
  match,
  parseMaybe,
 )
import Text.Megaparsec.Char (char, digitChar, hspace1, newline, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Extra (
  parsePretty,
 )

data Receipt = Receipt
  { _receiptDate :: !Day
  , _receiptEntries :: [Entry]
  , _receiptRabatt :: !(Maybe Rabatt)
  , _receiptTotal :: !Decimal
  , _receiptPayments :: NonEmpty Payment
  }

data Entry = Entry
  { entryName :: !Text
  , entryTotal :: !Decimal
  }

data PaymentMethod = TWINT | Mastercard | Supercash

paymentMethodToAccount :: PaymentMethod -> Text
paymentMethodToAccount TWINT = bcgeAccount
paymentMethodToAccount Mastercard = bcgeCCAccount
paymentMethodToAccount Supercash = Wallet.expensesOther

data Payment = Payment
  { paymentMethod :: !PaymentMethod
  , paymentTotal :: !Decimal
  }

type Parser = Parsec Void Text

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
  (string "TWINT" >> return TWINT)
    <|> (string "Mastercard" >> return Mastercard)
    <|> (string "Supercash" >> return Supercash)

paymentP :: Parser Payment
paymentP = do
  method <- paymentMethodP
  total <- case method of
    Supercash -> do
      hspace1
      void $ decimalP defaultDecimalFormat
      hspace1
      void $ decimalP (cashDecimalFormat NoChunkSep)
      hspace1
      decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
    _ -> do
      hspace1
      decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))
  return $ Payment method total

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  void $ manyTill anyLineP headerLineP
  (maybeEntries, (maybeRabatt, total)) <-
    manyTill_
      ((newline >> return Nothing) <|> (string "0\n" >> return Nothing) <|> (Just <$> entryLineP))
      maybeRabattAndTotalP
  void $ many newline
  Receipt day (catMaybes maybeEntries) maybeRabatt total <$> some1 (paymentP <* many newline)

entryNameToExpenseCategory :: Text -> Text
entryNameToExpenseCategory entry =
  fromMaybe
    "Groceries"
    ( listToMaybe (mapMaybe (\(rgx, cat) -> if has rgx entry then Just cat else Nothing) itemToExpenseCategoryPairs)
    )
 where
  haushalt = "Haushalt"
  coffee = "Groceries" <:> "Coffee"
  readyMeals = "Groceries" <:> "Ready Meals"
  alcohol = "Groceries" <:> "Alcohol"
  health = "Gesundheit"
  itemToExpenseCategoryPairs =
    [ ([regex|Stimorol|], "Groceries:Chewing Gum")
    , ([regex|Acqua Panna|], coffee)
    , ([regex|Naturaplan Espresso Havelaar Bohnen|], coffee)
    , ([regex|Emmi Caffè Latte|], coffee <:> "Latte")
    , ([regex|Salmon Poké|], readyMeals)
    , ([regex|ZENBU|], readyMeals)
    , ([regex|Findus Egli|], readyMeals)
    , ([regex|BeringerFounders|], alcohol)
    , ([regex|Cahors|], alcohol)
    , ([regex|Dôle|], alcohol)
    , ([regex|Gamay|], alcohol)
    , ([regex|Moscato|], alcohol)
    , ([regex|Nero|], alcohol)
    , ([regex|Wein|], alcohol)
    , ([regex|[tT]eelicht|], haushalt)
    , ([regex|WC Frisch|], haushalt)
    , ([regex|Haushalt|], haushalt)
    , ([regex|Schwamm|], haushalt)
    , ([regex|Desinfektionstücher|], haushalt)
    , ([regex|^À Table!|], haushalt)
    , ([regex|^Persil|], haushalt)
    , ([regex|^Flup|], haushalt)
    , ([regex|^Dr.Beckmann|], haushalt)
    , ([regex|^Sun|], haushalt)
    , ([regex|^Palmolive Ultra|], haushalt)
    , ([regex|Reinigung|], haushalt)
    , ([regex|Compo-Bag|], haushalt)
    , ([regex|Brita|], haushalt)
    , ([regex|ZEISS|], haushalt)
    , ([regex|Finish|], haushalt)
    , ([regex|Tesa|], haushalt)
    , ([regex|CR2032|], haushalt)
    , ([regex|Dübel|], haushalt)
    , ([regex|Listerine|], health)
    , ([regex|Nivea|], health)
    , ([regex|Mücken|], haushalt <:> "Mückenschutz")
    ]

paymentToPosting :: Payment -> Posting
paymentToPosting Payment{paymentMethod = method, paymentTotal = total} =
  Ledger.post
    (paymentMethodToAccount method)
    (HDE.makeCurrencyAmount chf (- total))
    & L.set pStatus (postingStatus method)
 where
  postingStatus Supercash = Unmarked
  postingStatus TWINT = Pending
  postingStatus Mastercard = Pending

-- | Splits a single receipt entry into an expense category and optional debtor accounts.
entryToPostings :: Config -> Entry -> ((AccountName, Decimal), [(AccountName, Decimal)])
entryToPostings (Config{shared = rules}) (Entry{entryName = name, entryTotal = total}) =
  ((expenseCategory, mainAlloc), zip debtors debtAllocs)
 where
  expenseCategory = expenses <:> entryNameToExpenseCategory name
  debtors = getDebtors rules name
  twoDigitTotal = realFracToDecimal 2 total
  (mainAlloc : debtAllocs) = D.normalizeDecimal <$> D.allocate twoDigitTotal (replicate (1 + length debtors) 1)

receiptToTransaction :: Config -> Receipt -> Transaction
receiptToTransaction config (Receipt day entries rabatt _total payments) =
  transaction day postings
    & L.set tDescription "Coop"
      . L.set tStatus Cleared
 where
  postings = toList paymentPostings <> catItems <> debtItems <> rabattPosting
  paymentPostings = paymentToPosting <$> payments
  (myExpenses :: [(AccountName, Decimal)], debtors :: [(AccountName, Decimal)]) =
    map (entryToPostings config) entries & unzip & fmap concat
  catToTotals :: HashMap Text (NonEmpty Decimal) = snd <<$>> groupBy fst myExpenses
  debtToTotals :: HashMap Text (NonEmpty Decimal) = snd <<$>> groupBy fst debtors
  [catToTotal, debtToTotal] = fmap sum <$> [catToTotals, debtToTotals]
  catItems =
    map
      ( \(cat, total') ->
          Ledger.post
            cat
            (HDE.makeCurrencyAmount chf total')
      )
      (HashMap.toList catToTotal)
  debtItems =
    map
      ( \(cat, total') ->
          Ledger.post
            cat
            (HDE.makeCurrencyAmount chf total')
            & L.set pStatus Pending
      )
      (HashMap.toList debtToTotal)
  rabattToPosting (Rabatt rabattVal) =
    Ledger.post
      "Expenses:Other"
      (HDE.makeCurrencyAmount chf rabattVal)
  rabattPosting = rabattToPosting <$> maybeToList rabatt

receiptToLedger :: Config -> Text -> Either Text Transaction
receiptToLedger config receiptText = do
  receipt <- parsePretty receiptP "a Coop receipt" receiptText
  return $ receiptToTransaction config receipt
