{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hledupt.Coop (
  receiptToLedger,
) where

import Control.Lens (
  has,
  lens,
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
import Relude.Extra (groupBy)
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
  , _receiptTotal :: !Decimal
  }

data Entry = Entry
  { _entryName :: !Text
  , _entryTotal :: !Decimal
  }

entryName :: L.Lens' Entry Text
entryName = lens _entryName (\entry name -> entry{_entryName = name})

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
                  ^? [regex|(.*) (\d+) (\d+\.\d\d)( \d+\.\d\d)? (\d+\.\d\d) \d+|] . LR.groups
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
      _ -> Nothing

totalLineP :: Parser Decimal
totalLineP = do
  void $ string "Total CHF "
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction))

receiptP :: Parser Receipt
receiptP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  void $ manyTill anyLineP headerLineP
  (maybeEntries, total) <-
    manyTill_
      ((newline >> return Nothing) <|> (Just <$> entryLineP))
      totalLineP
  return $ Receipt day (catMaybes maybeEntries) total

entryNameToExpenseCategory :: Text -> Text
entryNameToExpenseCategory entry =
  fromMaybe
    "Groceries"
    ( listToMaybe (mapMaybe (\(rgx, cat) -> if has rgx entry then Just cat else Nothing) itemToExpenseCategoryPairs)
    )
 where
  itemToExpenseCategoryPairs =
    [ ([regex|Stimorol|], "Groceries:Chewing Gum")
    , ([regex|Salmon Poké|], "Groceries:Ready Meals")
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
receiptToTransaction (Receipt day entries total) =
  transaction day postings
    & L.set tDescription "Coop"
      . L.set tStatus Cleared
 where
  postings = [bcgePosting] <> items
  bcgePosting =
    Ledger.post
      "Assets:Liquid:BCGE"
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

receiptToLedger :: Text -> Either Text Transaction
receiptToLedger receiptText = do
  receipt <- parseReceipt receiptText
  return $ receiptToTransaction receipt
