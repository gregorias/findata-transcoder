{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Transcoder.BcgeCC (
  rechnungToLedger,
) where

import qualified Control.Lens as L
import Control.Lens.Regex.Text (regex)
import qualified Control.Lens.Regex.Text as LR
import Data.Decimal (Decimal)
import Data.Time (Day)
import Data.Time.Extra (dayP)
import Hledger (
  Status (Cleared),
  Transaction,
  balassert,
  post,
  transaction,
 )
import qualified Hledger.Data.Extra as HDE
import Hledger.Data.Lens (pBalanceAssertion, tDescription, tStatus)
import Relude
import Relude.Extra.Map (lookup)
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  getParserState,
  label,
  manyTill,
  manyTill_,
  parseMaybe,
  updateParserState,
 )
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Extra (anyLineP)
import Text.Megaparsec.Extra (
  parsePretty,
 )
import Transcoder.Data.Currency (chf)
import Transcoder.Data.MyDecimal (
  ChunkSepFormat (ChunkSep, NoChunkSep),
  DecimalFormat (..),
  DecimalFractionFormat (TwoDigitDecimalFraction),
  decimalP,
 )
import Transcoder.Wallet (
  bcgeCCAccount,
  expensesOther,
  financialServices,
 )

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

dateLineP :: BcgeCCParser Day
dateLineP = label "invoice date line" $ do
  void $ string "Invoice of "
  dayP "%d.%m.%Y" <* newline

transactionP :: BcgeCCParser RawRechnungTransaction
transactionP = label "transaction" $ do
  (fstDay, sndDay) <- try $ do
    fstDay <- dayP "%d.%m.%y"
    void $ char ' '
    sndDay <- dayP "%d.%m.%y"
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
    sgn <-
      (string "Processing fee" >> return id)
        <|> (string "Credit of processing fee" >> return negate)
    void $ string " 1.75% CHF "
    fee <- decimalP (DecimalFormat NoChunkSep (Just TwoDigitDecimalFraction))
    void newline
    return . sgn $ fee
  toTitleAndAmount :: Text -> Maybe (Text, Text, Bool)
  toTitleAndAmount rol = do
    namedGroups <- maybeNamedGroups
    title <- lookup "title" namedGroups
    amt <- lookup "amountchf" namedGroups
    return (title, amt, isCredit)
   where
    regexes =
      [ [regex|(?<title>.*) (?<countrycode>[A-Z]{2}) (?<currency>[A-Z]{3}) (?<amount>[0-9']+\.\d\d) (?<amountchf>[0-9']+\.\d\d)|]
      , [regex|(?<title>.*) (?<currency>[A-Z]{3}) (?<amount>[0-9']+\.\d\d) (?<amountchf>[0-9']+\.\d\d)|]
      , [regex|(?<title>.*) (?<countrycode>[A-Z]{2}) (?<amountchf>[0-9']+\.\d\d)|]
      ]
    maybeNamedGroups :: (Maybe (Map Text Text)) = asum $ flip L.preview rol <$> fmap (. LR.namedGroups) regexes
    isCredit = isJust $ L.preview [regex| -$|] rol
  amtP :: Text -> Maybe Decimal
  amtP =
    parseMaybe @Void
      ( decimalP
          ( DecimalFormat
              { decimalFormatChunkSep = ChunkSep '\''
              , decimalFormatFractionFormat = Just TwoDigitDecimalFraction
              }
          )
      )
  titleAndAmountP :: BcgeCCParser (Text, Decimal)
  titleAndAmountP = label "transaction title and amount" $
    try $ do
      before <- getParserState
      rol :: Text <- anyLineP
      let (maybeTitleAndAmount :: Maybe (Text, Decimal)) = do
            (title, amtText, sign) <- toTitleAndAmount rol
            amt <- amtP amtText
            return (title, if sign then -amt else amt)
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
  decimalP (DecimalFormat (ChunkSep '\'') (Just TwoDigitDecimalFraction)) <* anyLineP

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
      [post bcgeCCAccount (HDE.makeCurrencyAmount chf (-amtInChf))]
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
        & L.set pBalanceAssertion (balassert . HDE.makeCurrencyAmount chf $ -owedAmount)

rechnungToLedger :: Text -> Either Text [Transaction]
rechnungToLedger rechnungTxt =
  rechnungToTransactions
    <$> parsePretty rechnungP "a BCGE CC statement" rechnungTxt
