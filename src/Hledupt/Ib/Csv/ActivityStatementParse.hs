{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module parses IB CSV activity statements.
module Hledupt.Ib.Csv.ActivityStatementParse
  ( -- * Types
    ActivityStatement (..),
    nullActivityStatement,
    CashMovement (..),
    Currency (..),
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
  )
where

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Csv as Csv hiding (FromNamedRecord, decodeByName, lookup)
import qualified Data.Csv as CsvLookup (lookup)
import qualified Data.Csv.Extra as Csv
import Data.List (isInfixOf)
import Data.List.Extra (allSame)
import qualified Data.Map.Strict as Map
import Data.Time (Day, defaultTimeLocale, fromGregorian, parseTimeM)
import qualified Data.Vector as V
import Hledupt.Data (MonetaryValue, decimalParser, myDecDec)
import Hledupt.Ib.Csv.RawParse
  ( Csv (..),
    Section (..),
    Statement (..),
  )
import Relude
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
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
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, numberChar, space, string)

data Currency = USD | CHF
  deriving stock (Eq, Show)

instance Csv.FromField Currency where
  parseField "USD" = pure USD
  parseField "CHF" = pure CHF
  parseField _ = fail "Expected CHF/USD as currency"

-- Statement info parsers

monthParser :: Parsec Void String Int
monthParser = do
  monthString <- some letterChar
  case monthString of
    "January" -> return 1
    "February" -> return 2
    "March" -> return 3
    "April" -> return 4
    "May" -> return 5
    "June" -> return 6
    "July" -> return 7
    "August" -> return 8
    "September" -> return 9
    "October" -> return 10
    "November" -> return 11
    "December" -> return 12
    _ -> mzero

datePhraseParser :: Parsec Void String Day
datePhraseParser = do
  month <- monthParser <* space
  day <- some digitChar <* string ", "
  year <- count 4 digitChar
  let date = do
        yearInt <- readMaybe year
        dayInt <- readMaybe day
        return $ fromGregorian yearInt month dayInt
  case date of
    Just d -> return d
    Nothing -> fail "Could not parse date"

periodPhraseParser :: Parsec Void String Day
periodPhraseParser = do
  void $ string "Period,\""
  date <-
    try (datePhraseParser >> string " - " >> datePhraseParser)
      <|> datePhraseParser
  void $ single '"'
  return date

statementDateParser :: Parsec Void String Day
statementDateParser = snd <$> someTill_ anySingle (try periodPhraseParser)

-- Positions AKA Status parsers

data StockPosition = StockPosition
  { spSymbol :: String,
    spQuantity :: Integer,
    spPrice :: MonetaryValue
  }
  deriving stock (Eq, Show)

instance Csv.FromNamedRecord StockPosition where
  parseNamedRecord = do
    StockPosition <$> Csv.lookup "Symbol"
      <*> Csv.lookup "Quantity"
      <*> (L.view myDecDec <$> Csv.lookup "Close Price")

newtype BaseCurrency = BaseCurrency String
  deriving newtype (Eq, Show)

newtype QuoteCurrency = QuoteCurrency String
  deriving newtype (Eq, Show)

data QuotePair = QuotePair BaseCurrency QuoteCurrency
  deriving stock (Eq, Show)

quotePairParser :: Parsec Void String QuotePair
quotePairParser = do
  base <- some alphaNumChar
  void $ single '.'
  quote <- some alphaNumChar
  MP.eof
  return $ QuotePair (BaseCurrency base) (QuoteCurrency quote)

instance Csv.FromField QuotePair where
  parseField =
    maybe (fail "Could not parse the quote pair") return
      . MP.parseMaybe quotePairParser
      . (fmap (chr . fromEnum) . BS.unpack)

data StockTrade = StockTrade
  { stockTradeDate :: Day,
    stockTradeSymbol :: String,
    stockTradeQuantity :: Integer,
    stockTradeAmount :: MonetaryValue,
    stockTradeFee :: MonetaryValue
  }
  deriving stock (Eq, Show)

data ForexTrade = ForexTrade
  { forexTradeDate :: Day,
    forexTradeQuotePair :: QuotePair,
    forexTradeQuantity :: Integer,
    forexTradePrice :: MonetaryValue,
    forexTradeTotalCost :: MonetaryValue,
    forexTradeFee :: MonetaryValue
  }
  deriving stock (Eq, Show)

data AssetCategory = Forex | Stocks
  deriving stock (Eq, Show)

instance Csv.FromField AssetCategory where
  parseField "Forex" = pure Forex
  parseField "Stocks" = pure Stocks
  parseField field =
    fail $
      "Could not parse asset category: " ++ (chr . fromEnum <$> BS.unpack field)

tradesDateTimeParser :: Parsec Void String Day
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
                ( const $
                    fail $
                      "Could not parse Date/Time: " ++ dtString ++ "."
                )
                return
                eitherDate
          )
      <*> Csv.lookup "Symbol"
      <*> (unQuantity <$> Csv.lookup "Quantity")
      <*> (L.view myDecDec <$> Csv.lookup "Proceeds")
      <*> (L.view myDecDec <$> Csv.lookup "Comm/Fee")

optionalQuotes :: (Ord e) => Parsec e String a -> Parsec e String a
optionalQuotes = between optionalQuote optionalQuote
  where
    optionalQuote = optional $ single '"'

newtype Quantity = Quantity
  { unQuantity :: Integer
  }

quantityParser :: Parsec Void String Quantity
quantityParser = optionalQuotes $ do
  negMod <- MP.try (char '-' >> pure negate) MP.<|> pure id
  quantityString <- catMaybes <$> some numberCharOrComma
  case readMaybe quantityString of
    Just quantity -> return $ Quantity $ negMod quantity
    Nothing -> fail $ "Could not parse the quantity: " ++ quantityString
  where
    numberCharOrComma :: (Ord e) => Parsec e String (Maybe Char)
    numberCharOrComma = (Just <$> numberChar) <|> (single ',' $> Nothing)

instance Csv.FromField Quantity where
  parseField field =
    maybe (fail $ "Could not parse the quantity: " ++ fieldStr) return
      . MP.parseMaybe quantityParser
      $ fieldStr
    where
      fieldStr = fmap (chr . fromEnum) . BS.unpack $ field

instance Csv.FromNamedRecord ForexTrade where
  parseNamedRecord =
    ForexTrade
      <$> ( do
              dtString <- Csv.lookup "Date/Time"
              let eitherDate = MP.parse tradesDateTimeParser "" dtString
              either
                ( const $
                    fail $
                      "Could not parse Date/Time: " ++ dtString ++ "."
                )
                return
                eitherDate
          )
      <*> Csv.lookup "Symbol"
      <*> (unQuantity <$> Csv.lookup "Quantity")
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
        (C.pack csv)
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
  { cmDate :: Day,
    cmCurrency :: Currency,
    cmAmount :: MonetaryValue
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
  { dDate :: Day,
    dSymbol :: String,
    dDividendPerShare :: MonetaryValue,
    dTotalAmount :: MonetaryValue
  }
  deriving stock (Eq, Show)

symbolParser ::
  (MonadParsec e s m, Token s ~ Char) =>
  m String
symbolParser = some letterChar

symbolDpsParser ::
  (MonadFail m, MonadParsec e s m, Token s ~ Char, Tokens s ~ String) =>
  m (String, MonetaryValue)
symbolDpsParser = do
  symbol <- symbolParser
  void $ label "ISIN" $ char '(' >> some alphaNumChar >> char ')'
  void $ string " Cash Dividend USD "
  dps <- decimalParser
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
      dividendAux date (symbol, dps) total = Dividend date symbol dps total
      dividend =
        dividendAux
          <$> (Csv.lookup "Date" >>= parseTimeM True defaultTimeLocale "%Y-%m-%d")
          <*> ( do
                  desc :: String <- Csv.lookup "Description"
                  let parsed = MP.parse symbolDpsParser "" desc
                  either
                    ( \err ->
                        fail $
                          "Could not parse (symbol, dps).\n" ++ show err
                    )
                    return
                    parsed
              )
          <*> (L.view myDecDec <$> Csv.lookup "Amount")

data WithholdingTax = WithholdingTax
  { wtDate :: Day,
    wtSymbol :: String,
    wtTotalAmount :: MonetaryValue
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
                    let parsed = MP.parse symbolParser "" desc
                    either
                      ( \err ->
                          fail $
                            "Could not parse symbol.\n" ++ show err
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
  { ecCurrency :: String,
    ecAmount :: MonetaryValue
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
    currencySummaryField <- Csv.lookup "Currency Summary"
    currencyField <- Csv.lookup "Currency"
    if currencySummaryField /= "Ending Cash" || currencyField == "Base Currency Summary"
      then return OtherCashReportRecord
      else
        EndingCashRecord . EndingCash currencyField
          <$> (L.view myDecDec <$> Csv.lookup "Total")

fetchSection :: String -> Statement -> Either String Section
fetchSection name =
  maybeToRight ("Could not find " ++ name ++ " among the CSVs.")
    . Map.lookup name
    . unStatement

fetchSectionOrEmpty :: String -> Statement -> Section
fetchSectionOrEmpty name (Statement stmt) = Map.findWithDefault emptySection name stmt
  where
    emptySection = Section [""]

prependErrorMessage :: String -> Either String a -> Either String a
prependErrorMessage err = L._Left L.%~ (errln ++)
  where
    errln = err ++ "\n"

parseCsv :: (Csv.FromNamedRecord a) => Csv -> Either String [a]
parseCsv (Csv csv) =
  if null csv
    then return []
    else V.toList . snd <$> Csv.decodeByName (C.pack csv)

parseNamedSection :: (Csv.FromNamedRecord a) => String -> Statement -> Either String [a]
parseNamedSection name stmt = do
  let csvs = unSection $ fetchSectionOrEmpty name stmt
  prependErrorMessage
    ("Could not parse " ++ name ++ " records.")
    $ fmap concat . mapM parseCsv $
      csvs

parseTradesCsv :: Csv -> Either String Trades
parseTradesCsv csv = do
  cat <- detectTradesCsvCategory csv
  case cat of
    Stocks ->
      prependErrorMessage "Could not parse stock trades." $
        Trades <$> parseCsv csv <*> pure []
    Forex ->
      prependErrorMessage "Could not parse forex trades." $
        Trades [] <$> parseCsv csv

parseTradesSection :: Statement -> Either String Trades
parseTradesSection stmt = do
  let tradesName = "Trades"
      csvs = unSection $ fetchSectionOrEmpty tradesName stmt
  prependErrorMessage
    ("Could not parse " ++ tradesName ++ " records.")
    $ fold
      <$> (mapM parseTradesCsv :: NonEmpty Csv -> Either String (NonEmpty Trades))
        (csvs :: NonEmpty Csv)

-- | Useful information gleaned directly from IB's CSV activity statement.
data ActivityStatement = ActivityStatement
  { asLastStatementDay :: Day,
    asCashPositions :: [EndingCash],
    asStockPositions :: [StockPosition],
    asCashMovements :: [CashMovement],
    asStockTrades :: [StockTrade],
    asForexTrades :: [ForexTrade],
    asDividends :: [Dividend],
    asTaxes :: [WithholdingTax]
  }
  deriving stock (Eq, Show)

nullActivityStatement :: Day -> ActivityStatement
nullActivityStatement date = ActivityStatement date [] [] [] [] [] [] []

parseActivityStatement :: Statement -> Either String ActivityStatement
parseActivityStatement csvs = do
  date <- do
    stmtCsv <- head . unSection <$> fetchSection "Statement" csvs
    first errorBundlePretty $ MP.parse statementDateParser "" (unCsv stmtCsv)
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
  return $
    ActivityStatement
      date
      cashPositions
      stockPositions
      cashTransfers
      stockTrades
      forexTrades
      dividends
      taxes
