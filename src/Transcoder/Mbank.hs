module Transcoder.Mbank (
  MbankTransaction (..),
  mTrToLedger,
  mbankCsvToLedger,
  valueP,
  decodeMbankCsv,
  pln,
) where

import Data.ByteString.Lazy as LBS
import Data.Csv (DecodeOptions (DecodeOptions), FromNamedRecord (..), decodeByNameWith, lookup)
import Data.Csv qualified as CSV
import Data.Decimal (Decimal)
import Data.Decimal.Extra (fromUnitsAndCents)
import Data.Text qualified as T
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector (Vector)
import Hledger (missingamt, post)
import Hledger.Data.Extra (makeCurrencyAmount)
import Hledger.Data.Posting (
  balassert,
  post',
 )
import Hledger.Data.Transaction (transaction)
import Hledger.Data.Types (
  Amount (..),
  Quantity,
  Transaction (..),
 )
import Relude
import Text.Megaparsec (
  Parsec,
  oneOf,
  parseMaybe,
 )
import Text.Megaparsec.Char (char, string)
import Transcoder.Data.Currency qualified as Currency
import Transcoder.Data.LedgerReport (LedgerReport (LedgerReport))

pln :: Quantity -> Amount
pln = makeCurrencyAmount Currency.pln

-- | mBank's transaction data fetched from their website.
data MbankTransaction = MbankTransaction
  { mTrDate :: !Day
  , mTrTitle :: !Text
  , mTrAmount :: !Decimal
  , mTrEndBalance :: !Decimal
  }
  deriving stock (Eq, Show)

instance FromNamedRecord MbankTransaction where
  parseNamedRecord r = do
    dayText :: Text <- lookup r "#Data operacji"
    day <-
      maybe
        (fail $ "Could not parse " <> toString dayText <> " as a day.")
        return
        (parseMaybe dayP dayText)
    title <- lookup r "#Opis operacji"
    amount <- parseAmount =<< lookup r "#Kwota"
    endBalance <- parseAmount =<< lookup r "#Saldo po operacji"
    return
      MbankTransaction
        { mTrDate = day
        , mTrTitle = title
        , mTrAmount = amount
        , mTrEndBalance = endBalance
        }
   where
    parseAmount :: Text -> CSV.Parser Decimal
    parseAmount amountText =
      maybe
        (fail $ "Could not parse " <> toString amountText <> " as an amount.")
        return
        (parseMaybe valueP amountText)

-- Parser functionality (CSV String → [MbankTransaction])

type Parser = Parsec () Text

dayP :: Parser Day
dayP = do
  dateString <- many $ oneOf ("-0123456789" :: [Char])
  parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString

valueP :: Parser Decimal
valueP = do
  zloteString <- removeSpaces <$> many (oneOf ("-0123456789 " :: [Char]))
  zlote :: Integer <- case readMaybe zloteString of
    Just z -> return z
    Nothing -> fail ""
  groszeString <- (char ',' *> many (oneOf ['0' .. '9']) <* char ' ') <|> pure "00"
  grosze :: Integer <- case readMaybe groszeString of
    Just g -> return g
    Nothing -> fail ""
  void $ string "PLN"
  return $ fromUnitsAndCents zlote grosze
 where
  removeSpaces = Relude.filter (' ' /=)

-- | Decodes transactions from an mBank CSV.
decodeMbankCsv ::
  LBS.ByteString ->
  Either String (Vector MbankTransaction)
decodeMbankCsv =
  fmap snd
    . decodeByNameWith (DecodeOptions (fromIntegral (ord ';')))

-- MbankTransaction to Ledger transformers

sanitizeTitle :: Text -> Text
sanitizeTitle = fst . T.breakOn "   "

mTrToLedger :: MbankTransaction -> Transaction
mTrToLedger mTr = tr{tdescription = toText $ sanitizeTitle $ mTrTitle mTr}
 where
  tr =
    transaction
      (mTrDate mTr)
      [ post' "Assets:Liquid:mBank" (pln $ mTrAmount mTr) (balassert $ pln $ mTrEndBalance mTr)
      , post "Expenses:Other" missingamt
      ]

mbankCsvToLedger :: LBS.ByteString -> Either Text LedgerReport
mbankCsvToLedger inputCsv = do
  let parserErrorToString err =
        "Could not parse mBank's CSV.\n" <> show err
  mtransactions <-
    first (parserErrorToString . toText)
      $ ( fmap snd
            . decodeByNameWith (DecodeOptions (fromIntegral (ord ';')))
        )
        inputCsv
  let sortedMTransactions = sortOn mTrDate (toList mtransactions)
      ltransactions = fmap mTrToLedger sortedMTransactions
  return $ LedgerReport ltransactions []
