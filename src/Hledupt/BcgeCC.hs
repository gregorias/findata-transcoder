{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Hledupt.BcgeCC (
  rechnungToLedger,
) where

import Control.Lens (mapped)
import qualified Control.Lens as L
import Control.Lens.Regex.Text (regex)
import qualified Control.Lens.Regex.Text as LR
import Data.Decimal (Decimal)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Hledger (
  Status (Cleared),
  Transaction,
  balassert,
  post,
  transaction,
 )
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pBalanceAssertion, tDescription, tStatus)
import Hledupt.Data.Currency (chf)
import Hledupt.Data.MyDecimal (
  ChunkSepFormat (NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
  defaultDecimalFormat,
 )
import Hledupt.Wallet (
  bcgeCCAccount,
  expensesOther,
  financialServices,
 )
import Relude
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  anySingle,
  count,
  getParserState,
  label,
  manyTill,
  manyTill_,
  match,
  parse,
  parseMaybe,
  updateParserState,
 )
import Text.Megaparsec.Char (char, digitChar, newline, string)
import Text.Megaparsec.Error (errorBundlePretty)

data Rechnung = Rechnung
  { rechnungDay :: !Day
  , rechnungTransactions :: [RawRechnungTransaction]
  , rechnungOwedAmount :: !Decimal
  }

data RawRechnungTransaction = RawRechnungTransaction
  { rrtDay :: !Day
  , rrtAuxDay :: !Day
  , rrtTitle :: !Text
  , rrtAmountInChf :: !Decimal
  , rrtCategory :: !Text
  , rrtProcessingFeeInChf :: !(Maybe Decimal)
  }

type BcgeCCParser = Parsec Void Text

anyLineP :: BcgeCCParser Text
anyLineP = label "an arbitrary line" $ toText <$> manyTill anySingle newline

dateP :: String -> BcgeCCParser Day
dateP fmt = label ("date (" <> fmt <> ")") $ do
  (dateString, _) <-
    try $
      match
        (count 2 digitChar >> char '.' >> count 2 digitChar >> char '.' >> many digitChar)
  parseTimeM False defaultTimeLocale fmt (toString dateString)

dateLineP :: BcgeCCParser Day
dateLineP = label "invoice date line" $ do
  void $ string "Invoice of "
  dateP "%d.%m.%Y" <* newline

transactionP :: BcgeCCParser RawRechnungTransaction
transactionP = label "transaction" $ do
  (fstDay, sndDay) <- try $ do
    fstDay <- dateP "%d.%m.%y"
    void $ char ' '
    sndDay <- dateP "%d.%m.%y"
    return (fstDay, sndDay)
  void $ char ' '
  (title, amt) <- titleAndAmountP
  category <- anyLineP
  void . optional $ exchangeRateP
  processingFee <- optional processingFeeP
  return $
    RawRechnungTransaction
      { rrtDay = fstDay
      , rrtAuxDay = sndDay
      , rrtTitle = title
      , rrtAmountInChf = amt
      , rrtCategory = category
      , rrtProcessingFeeInChf = processingFee
      }
 where
  exchangeRateP = label "exchange rate line" $ do
    void $ string "Exchange rate "
    anyLineP
  processingFeeP = label "processing fee line" $ do
    void $ string "Processing fee" <|> string "Credit of processing fee"
    void $ string " 1.75% CHF "
    decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction)) <* newline
  toTitleAndAmount :: Text -> Maybe (Text, Text)
  toTitleAndAmount rol =
    asum
      [ ( \case
            [title, _, _, _, amt] -> Just (title, amt)
            _ -> Nothing
        )
          =<< L.preview
            ( [regex|(?<title>.*) (?<countrycode>[A-Z]{2}) (?<currency>[A-Z]{3}) (?<amount>\d+\.\d\d) (?<amountchf>\d+\.\d\d)|]
                . LR.groups
            )
            rol
      , ( \case
            [title, _, _, amt] -> Just (title, amt)
            _ -> Nothing
        )
          =<< L.preview
            ( [regex|(?<title>.*) (?<currency>[A-Z]{3}) (?<amount>\d+\.\d\d) (?<amountchf>\d+\.\d\d)|]
                . LR.groups
            )
            rol
      , ( \case
            [title, _, amt] -> Just (title, amt)
            _ -> Nothing
        )
          =<< L.preview
            ( [regex|(?<title>.*) (?<countrycode>[A-Z]{2}) (?<amountchf>\d+\.\d\d)|]
                . LR.groups
            )
            rol
      ]
  amtP :: Text -> Maybe Decimal
  amtP = parseMaybe @Void (decimalP defaultDecimalFormat)
  titleAndAmountP :: BcgeCCParser (Text, Decimal)
  titleAndAmountP = label "transaction title and amount" $
    try $ do
      before <- getParserState
      rol :: Text <- anyLineP
      let (maybeTitleAndAmount :: Maybe (Text, Decimal)) =
            join
              . L.over mapped sequence
              . L.over (mapped . L._2) amtP
              . toTitleAndAmount
              $ rol
      maybe
        (updateParserState (const before) >> empty)
        return
        maybeTitleAndAmount

-- [regex|(.*) (\d+|\d+\.\d+) (\d+\.\d\d)( \d+\.\d\d)? (\d+\.\d\d)( \d+)?|]
-- preview (r . namedGroups) "asdf CH EUR 0.00 0.00"
transactionsP :: BcgeCCParser [RawRechnungTransaction]
transactionsP = catMaybes <$> manyTill transactionOrExtraP totalCardLineP
 where
  totalCardLineP = void $ string "Total card" >> anyLineP
  transactionOrExtraP =
    Just <$> transactionP <|> (anyLineP >> return Nothing)

totalOwedAmountP :: BcgeCCParser Decimal
totalOwedAmountP = label "total owed amount line" $ do
  void $ string "Total balance in our favour "
  decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction)) <* anyLineP

rechnungP :: BcgeCCParser Rechnung
rechnungP = do
  (_, day) <- manyTill_ anyLineP dateLineP
  void $ manyTill_ anyLineP cardLimitP
  trs <- transactionsP
  (_, owedAmount) <- manyTill_ anyLineP totalOwedAmountP
  void $ many anyLineP
  return $
    Rechnung
      { rechnungDay = day
      , rechnungTransactions = trs
      , rechnungOwedAmount = owedAmount
      }
 where
  cardLimitP = void $ string "Card limit CHF" >> anyLineP

parseRechnung :: Text -> Either Text Rechnung
parseRechnung rechnung = prepareErrMsg parsedReceipt
 where
  parsedReceipt = parse rechnungP "" rechnung
  prepareErrMsg =
    first (("Could not parse the payslip.\n" <>) . (toText . errorBundlePretty))

rawRechnungTransactionToTransaction :: RawRechnungTransaction -> Transaction
rawRechnungTransactionToTransaction
  RawRechnungTransaction
    { rrtDay = day
    , rrtTitle = title
    , rrtAmountInChf = amtInChf
    , rrtProcessingFeeInChf = processingFeeInChf
    } =
    transaction day postings
      & L.set tStatus Cleared
        . L.set tDescription title
   where
    makeAFeePosting fee = post financialServices (HDE.makeCurrencyAmount chf fee)
    feeOrZero = fromMaybe 0 processingFeeInChf
    postings =
      [post bcgeCCAccount (HDE.makeCurrencyAmount chf (- amtInChf))]
        <> maybeToList (makeAFeePosting <$> processingFeeInChf)
        <> [post expensesOther (HDE.makeCurrencyAmount chf (amtInChf - feeOrZero))]

rechnungToTransactions :: Rechnung -> [Transaction]
rechnungToTransactions
  Rechnung
    { rechnungDay = day
    , rechnungTransactions = trs
    , rechnungOwedAmount = owedAmount
    } =
    (rawRechnungTransactionToTransaction <$> trs)
      <> [ transaction day [balancePosting]
            & L.set tStatus Cleared
              . L.set tDescription "BCGE CC Status"
         ]
   where
    balancePosting =
      post bcgeCCAccount (HDE.makeCurrencyAmount chf 0)
        & L.set pBalanceAssertion (balassert . HDE.makeCurrencyAmount chf $ owedAmount)

rechnungToLedger :: Text -> Either Text [Transaction]
rechnungToLedger rechnungTxt = rechnungToTransactions <$> parseRechnung rechnungTxt
