{-# LANGUAGE OverloadedLists #-}

-- | This module parses IB CSV activity statements.
module Transcoder.Ib.Csv.ActivityStatementParse (
  -- * Types
  ActivityStatement (..),
  nullActivityStatement,
  CashMovement (..),
  Dividend (..),
  EndingCash (..),
  StockPosition (..),
  WithholdingTax (..),

  -- ** Trade types
  ForexTrade (..),
  StockTrade (..),
  BaseCurrency (..),
  QuoteCurrency (..),
  QuotePair (..),

  -- * Parsing
  parseActivityStatement,
) where

import Control.Lens qualified as L
import Data.Csv qualified as Csv hiding (FromNamedRecord, decodeByName, lookup)
import Data.Csv qualified as CsvLookup (lookup)
import Data.Csv.Extra qualified as Csv
import Data.Decimal (Decimal)
import Data.Decimal.Extra (decimalP, defaultDecimalFormat)
import Data.Either.Extra (mapLeft)
import Data.List (isInfixOf)
import Data.List.Extra (allSame)
import Data.Map.Strict qualified as Map
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM)
import Data.Time.Calendar.Extra (monthP)
import Data.Vector qualified as V
import Relude
import Text.Megaparsec (
  MonadParsec,
  Parsec,
  ParsecT,
  Token,
  Tokens,
  anySingle,
  between,
  count,
  errorBundlePretty,
  label,
  single,
  someTill_,
  try,
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, numberChar, space, string)
import Transcoder.Data.Currency (Currency, currencyP)
import Transcoder.Data.MyDecimal (myDecDec)
import Transcoder.Ib.Csv.RawParse (
  Csv (..),
  Section (..),
  Statement (..),
 )

datePhraseParser :: Parsec Void Text Day
datePhraseParser = do
  month <- monthP <* space
  day <- some digitChar <* string ", "
  year <- count 4 digitChar
  let date = do
        yearInt <- readMaybe year
        dayInt <- readMaybe day
        return $ fromGregorian yearInt month dayInt
  case date of
    Just d -> return d
    Nothing -> fail "Could not parse date"

periodPhraseParser :: Parsec Void Text Day
periodPhraseParser = do
  void $ string "Period,\""
  date <-
    try (datePhraseParser >> string " - " >> datePhraseParser)
      <|> datePhraseParser
  void $ single '"'
  return date

statementDateParser :: Parsec Void Text Day
statementDateParser = snd <$> someTill_ anySingle (try periodPhraseParser)

-- Positions AKA Status parsers

data StockPosition = StockPosition
  { spSymbol :: !Text
  , spQuantity :: !Integer
  , spPrice :: !Decimal
  }
  deriving stock (Eq, Show)

instance Csv.FromNamedRecord StockPosition where
  parseNamedRecord = do
    StockPosition
      <$> Csv.lookup "Symbol"
      <*> Csv.lookup "Quantity"
      <*> (L.view myDecDec <$> Csv.lookup "Close Price")

newtype BaseCurrency = BaseCurrency Currency
  deriving newtype (Eq, Show)

newtype QuoteCurrency = QuoteCurrency Currency
  deriving newtype (Eq, Show)

data QuotePair = QuotePair BaseCurrency QuoteCurrency
  deriving stock (Eq, Show)

quotePairParser :: Parsec Void Text QuotePair
quotePairParser = do
  base <- currencyP
  void $ single '.'
  quote <- currencyP
  MP.eof
  return $ QuotePair (BaseCurrency base) (QuoteCurrency quote)

instance Csv.FromField QuotePair where
  parseField =
    maybe (fail "Could not parse the quote pair") return
      . MP.parseMaybe quotePairParser
      . (toText . fmap (chr . fromEnum) . decodeUtf8 @String)

data StockTrade = StockTrade
  { stockTradeDate :: !Day
  , stockTradeSymbol :: !Text
  , stockTradeQuantity :: !Integer
  , stockTradeAmount :: !Decimal
  , stockTradeFee :: !Decimal
  }
  deriving stock (Eq, Show)

data ForexTrade = ForexTrade
  { forexTradeDate :: !Day
  , forexTradeQuotePair :: !QuotePair
  , forexTradeQuantity :: !Decimal
  , forexTradePrice :: !Decimal
  , forexTradeTotalCost :: !Decimal
  , forexTradeFee :: !Decimal
  }
  deriving stock (Eq, Show)

data AssetCategory = Forex | Stocks
  deriving stock (Eq, Show)

instance Csv.FromField AssetCategory where
  parseField "Forex" = pure Forex
  parseField "Stocks" = pure Stocks
  parseField field =
    fail
      $ "Could not parse asset category: "
      <> (chr . fromEnum <$> decodeUtf8 @String field)

tradesDateTimeParser :: Parsec Void Text Day
tradesDateTimeParser =
  MP.manyTill anySingle (single ',')
    >>= parseTimeM True defaultTimeLocale "%Y-%m-%d"

instance Csv.FromNamedRecord StockTrade where
  parseNamedRecord =
    StockTrade
      <$> ( do
              dtString <- Csv.lookup "Date/Time"
              let eitherDate = MP.parse tradesDateTimeParser "" dtString
              either
                ( const
                    $ fail
                    . toString
                    $ "Could not parse Date/Time: "
                    <> dtString
                    <> "."
                )
                return
                eitherDate
          )
      <*> Csv.lookup "Symbol"
      <*> (unQuantity <$> Csv.lookup "Quantity")
      <*> (L.view myDecDec <$> Csv.lookup "Proceeds")
      <*> (L.view myDecDec <$> Csv.lookup "Comm/Fee")

optionalQuotes :: (Ord e) => Parsec e Text a -> Parsec e Text a
optionalQuotes = between optionalQuote optionalQuote
 where
  optionalQuote = optional $ single '"'

newtype Quantity = Quantity
  { unQuantity :: Integer
  }

quantityParser :: Parsec Void Text Quantity
quantityParser = optionalQuotes $ do
  negMod <- MP.try (char '-' >> pure negate) MP.<|> pure id
  quantityString <- catMaybes <$> some numberCharOrComma
  case readMaybe quantityString of
    Just quantity -> return $ Quantity $ negMod quantity
    Nothing -> fail $ "Could not parse the quantity: " <> quantityString
 where
  numberCharOrComma :: (Ord e) => Parsec e Text (Maybe Char)
  numberCharOrComma = (Just <$> numberChar) <|> (single ',' $> Nothing)

instance Csv.FromField Quantity where
  parseField field =
    maybe (fail . toString $ "Could not parse the quantity: " <> fieldStr) return
      . MP.parseMaybe quantityParser
      $ fieldStr
   where
    fieldStr = toText . fmap (chr . fromEnum) . decodeUtf8 @String $ field

instance Csv.FromNamedRecord ForexTrade where
  parseNamedRecord =
    ForexTrade
      <$> ( do
              dtString <- Csv.lookup "Date/Time"
              let eitherDate = MP.parse tradesDateTimeParser "" dtString
              either
                ( const
                    $ fail
                    . toString
                    $ "Could not parse Date/Time: "
                    <> dtString
                    <> "."
                )
                return
                eitherDate
          )
      <*> Csv.lookup "Symbol"
      <*> (L.view myDecDec <$> Csv.lookup "Quantity")
      <*> (L.view myDecDec <$> Csv.lookup "T. Price")
      <*> (L.view myDecDec <$> Csv.lookup "Proceeds")
      <*> (L.view myDecDec <$> Csv.lookup "Comm in CHF")

detectTradesCsvCategory :: Csv -> Either String AssetCategory
detectTradesCsvCategory (Csv csv)
  | null csv = return Stocks
  | otherwise = do
      (_header, cats) <-
        Csv.decodeByNameWithP
          catParser
          Csv.defaultDecodeOptions
          (encodeUtf8 csv)
      extractCat $ toList cats
 where
  catParser nr = CsvLookup.lookup nr "Asset Category"
  extractCat :: [AssetCategory] -> Either String AssetCategory
  extractCat cats = do
    unless (allSame cats) $ Left "A Trades CSV has different categories"
    case cats of
      [] -> return Stocks
      (x : _) -> return x

data Trades = Trades [StockTrade] [ForexTrade]

instance Semigroup Trades where
  (<>) (Trades lss lfs) (Trades rss rfs) = Trades (lss <> rss) (lfs <> rfs)

instance Monoid Trades where
  mempty = Trades [] []

data CashMovement = CashMovement
  { cmDate :: Day
  , cmCurrency :: Currency
  , cmAmount :: Decimal
  }
  deriving stock (Eq, Show)

newtype CashMovementRecord = CashMovementRecord
  { unCashMovementRecord :: Maybe CashMovement
  }
  deriving stock (Eq, Show)

instance Csv.FromNamedRecord CashMovementRecord where
  parseNamedRecord =
    (CashMovementRecord . Just <$> cashMovement)
      <|> pure (CashMovementRecord Nothing)
   where
    cashMovement =
      CashMovement
        <$> (Csv.lookup "Settle Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
        <*> Csv.lookup "Currency"
        <*> (L.view myDecDec <$> Csv.lookup "Amount")

data Dividend = Dividend
  { dDate :: Day
  , dSymbol :: String
  , dDividendPerShare :: Decimal
  , dTotalAmount :: Decimal
  }
  deriving stock (Eq, Show)

symbolParser ::
  (MonadParsec e s m, Token s ~ Char) =>
  m String
symbolParser = some letterChar

symbolDpsParser ::
  (Tokens s ~ String, Tokens s ~ s) => ParsecT Void s m (String, Decimal)
symbolDpsParser = do
  symbol <- symbolParser
  void $ label "ISIN" $ char '(' >> some alphaNumChar >> char ')'
  void $ string " Cash Dividend USD "
  dps <- decimalP defaultDecimalFormat
  void $ string " per Share (Ordinary Dividend)"
  return (symbol, dps)

data DividendRecord
  = DividendRecord Dividend
  | TotalDividendsRecord
  deriving stock (Eq, Show)

_DividendRecord :: L.Prism' DividendRecord Dividend
_DividendRecord =
  L.prism'
    DividendRecord
    ( \case
        TotalDividendsRecord -> Nothing
        DividendRecord dividend -> Just dividend
    )

instance Csv.FromNamedRecord DividendRecord where
  parseNamedRecord = do
    currencyField <- Csv.lookup "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalDividendsRecord
      else fmap DividendRecord dividend
   where
    dividendAux date (symbol, dps) = Dividend date symbol dps
    dividend =
      dividendAux
        <$> (Csv.lookup "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
        <*> ( do
                desc :: String <- Csv.lookup "Description"
                let parsed = MP.parse symbolDpsParser "" desc
                either
                  ( \err ->
                      fail
                        $ "Could not parse (symbol, dps).\n"
                        <> show err
                  )
                  return
                  parsed
            )
        <*> (L.view myDecDec <$> Csv.lookup "Amount")

data WithholdingTax = WithholdingTax
  { wtDate :: Day
  , wtSymbol :: String
  , wtTotalAmount :: Decimal
  }
  deriving stock (Eq, Show)

-- | A record that appears in "Withholding Tax" CSV
data WithholdingTaxRecord
  = WithholdingTaxRecord WithholdingTax
  | TotalWithholdingTaxRecord
  deriving stock (Eq, Show)

instance Csv.FromNamedRecord WithholdingTaxRecord where
  parseNamedRecord = do
    currencyField <- Csv.lookup "Currency"
    if "Total" `isInfixOf` currencyField
      then return TotalWithholdingTaxRecord
      else WithholdingTaxRecord <$> withholdingTax
   where
    withholdingTax =
      do
        WithholdingTax
        <$> (Csv.lookup "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
        <*> ( do
                desc :: String <- Csv.lookup "Description"
                let parsed = MP.parse @Void symbolParser "" desc
                either
                  ( \err ->
                      fail
                        $ "Could not parse symbol.\n"
                        <> show err
                  )
                  return
                  parsed
            )
        <*> (L.view myDecDec <$> Csv.lookup "Amount")

_WithholdingTaxRecord :: L.Prism' WithholdingTaxRecord WithholdingTax
_WithholdingTaxRecord =
  L.prism'
    WithholdingTaxRecord
    ( \case
        TotalWithholdingTaxRecord -> Nothing
        WithholdingTaxRecord tax -> Just tax
    )

data EndingCash = EndingCash
  { ecCurrency :: Currency
  , ecAmount :: Decimal
  }
  deriving stock (Eq, Show)

data CashReportRecord
  = EndingCashRecord EndingCash
  | OtherCashReportRecord
  deriving stock (Eq, Show)

unCashReportRecord :: CashReportRecord -> Maybe EndingCash
unCashReportRecord OtherCashReportRecord = Nothing
unCashReportRecord (EndingCashRecord ec) = Just ec

instance Csv.FromNamedRecord CashReportRecord where
  parseNamedRecord = do
    currencySummaryField :: String <- Csv.lookup "Currency Summary"
    currencyField <- Csv.lookup "Currency"
    if currencySummaryField /= "Ending Cash" || currencyField == "Base Currency Summary"
      then return OtherCashReportRecord
      else do
        Just currency <- return $ readMaybe currencyField
        EndingCashRecord
          . EndingCash currency
          <$> (L.view myDecDec <$> Csv.lookup "Total")

fetchSection :: String -> Statement -> Either String Section
fetchSection name =
  maybeToRight ("Could not find " <> name <> " among the CSVs.")
    . Map.lookup name
    . unStatement

fetchSectionOrEmpty :: String -> Statement -> Section
fetchSectionOrEmpty name (Statement stmt) = Map.findWithDefault emptySection name stmt
 where
  emptySection = Section [""]

prependErrorMessage :: String -> Either String a -> Either String a
prependErrorMessage err = L._Left L.%~ (errln <>)
 where
  errln = err <> "\n"

parseCsv :: (Csv.FromNamedRecord a) => Csv -> Either String [a]
parseCsv (Csv csv) =
  if null csv
    then return []
    else V.toList . snd <$> Csv.decodeByName (encodeUtf8 csv)

parseNamedSection :: (Csv.FromNamedRecord a) => String -> Statement -> Either String [a]
parseNamedSection name stmt = do
  let csvs = unSection $ fetchSectionOrEmpty name stmt
  prependErrorMessage
    ("Could not parse " <> name <> " records.")
    $ fmap concat
    . mapM parseCsv
    $ csvs

parseTradesCsv :: Csv -> Either String Trades
parseTradesCsv csv = do
  cat <- detectTradesCsvCategory csv
  case cat of
    Stocks ->
      prependErrorMessage "Could not parse stock trades."
        $ Trades
        <$> parseCsv csv
        <*> pure []
    Forex ->
      prependErrorMessage "Could not parse forex trades."
        $ Trades []
        <$> parseCsv csv

parseTradesSection :: Statement -> Either String Trades
parseTradesSection stmt = do
  let tradesName = "Trades"
      csvs = unSection $ fetchSectionOrEmpty tradesName stmt
  prependErrorMessage
    ("Could not parse " <> tradesName <> " records.")
    $ fold
    <$> (mapM parseTradesCsv :: NonEmpty Csv -> Either String (NonEmpty Trades))
      (csvs :: NonEmpty Csv)

-- | Useful information gleaned directly from IB's CSV activity statement.
data ActivityStatement = ActivityStatement
  { asLastStatementDay :: !Day
  , asCashPositions :: ![EndingCash]
  , asStockPositions :: ![StockPosition]
  , asCashMovements :: ![CashMovement]
  , asStockTrades :: ![StockTrade]
  , asForexTrades :: ![ForexTrade]
  , asDividends :: ![Dividend]
  , asTaxes :: ![WithholdingTax]
  }
  deriving stock (Eq, Show)

nullActivityStatement :: Day -> ActivityStatement
nullActivityStatement date = ActivityStatement date [] [] [] [] [] [] []

parseActivityStatement :: Statement -> Either Text ActivityStatement
parseActivityStatement csvs = mapLeft toText $ do
  date <- do
    stmtCsv <- head . unSection <$> fetchSection "Statement" csvs
    first errorBundlePretty $ MP.parse statementDateParser "" (toText $ unCsv stmtCsv)
  cashPositions <-
    mapMaybe unCashReportRecord
      <$> parseNamedSection "Cash Report" csvs
  stockPositions <- parseNamedSection "Open Positions" csvs
  Trades stockTrades forexTrades <- parseTradesSection csvs
  cashTransfers <-
    mapMaybe unCashMovementRecord
      <$> parseNamedSection "Deposits & Withdrawals" csvs
  dividends <-
    mapMaybe (L.preview _DividendRecord)
      <$> parseNamedSection "Dividends" csvs
  taxes <-
    mapMaybe (L.preview _WithholdingTaxRecord)
      <$> parseNamedSection "Withholding Tax" csvs
  return
    $ ActivityStatement
      date
      cashPositions
      stockPositions
      cashTransfers
      stockTrades
      forexTrades
      dividends
      taxes
