{-# LANGUAGE UndecidableInstances #-}

-- | This module parses Degiro's account statement into Ledger.
module Hledupt.Degiro.AccountStatement (
  csvStatementToLedger,
  csvRecordsToLedger,
) where

import Control.Lens (set, view)
import qualified Data.ByteString.Lazy as LBS
import Data.Decimal (Decimal)
import Data.Either.Combinators (
  mapLeft,
 )
import Data.Ratio ((%))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.LocalTime (TimeOfDay)
import Hledger (
  AmountPrice (UnitPrice),
  Status (Cleared, Pending),
  Transaction,
  amountSetFullPrecision,
  balassert,
  post,
  transaction,
 )
import Hledger.Data (Posting)
import Hledger.Data.Extra (makeCashAmount, makeCommodityAmount)
import Hledger.Data.Lens (
  aAmountPrice,
  pAmount,
  pBalanceAssertion,
  pStatus,
  tDescription,
  tStatus,
 )
import Hledupt.Data.Cash (Cash (Cash), cashAmount, cashCurrency)
import qualified Hledupt.Data.Cash as Cash
import Hledupt.Data.CsvFile (CsvFile)
import Hledupt.Data.Currency (Currency, currencyP)
import Hledupt.Data.Isin (Isin, isin)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Data.MyDecimal (
  decimalP,
  defaultDecimalFormat,
 )
import Hledupt.Degiro.Csv (
  DegiroCsvRecord (..),
  parseCsvStatement,
 )
import Hledupt.Degiro.IsinData (prettyIsin)
import Relude
import Text.Megaparsec (
  MonadParsec (eof, label, token),
  Parsec,
  Stream,
  VisualStream,
  anySingle,
  choice,
  customFailure,
  manyTill,
  parse,
  single,
 )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Extra.ErrorText (ErrorText (..))

moneyMarketIsin :: Isin
moneyMarketIsin = [isin|NL0011280581|]

data Deposit = Deposit
  { _depositDate :: Day
  , _depositTime :: TimeOfDay
  , _depositAmount :: Cash
  , _depositBalance :: Cash
  }

depositP :: DegiroCsvRecord -> Maybe Deposit
depositP rec
  | dcrDescription rec /= "Deposit" = Nothing
  | otherwise = do
    change <- dcrChange rec
    return $ Deposit (dcrDate rec) (dcrTime rec) change (dcrBalance rec)

withdrawalP :: DegiroCsvRecord -> Maybe Deposit
withdrawalP rec
  | dcrDescription rec /= "Withdrawal" = Nothing
  | otherwise = do
    change <- dcrChange rec
    return $ Deposit (dcrDate rec) (dcrTime rec) change (dcrBalance rec)

depositToTransaction :: Deposit -> Transaction
depositToTransaction (Deposit date _time amount balance) =
  transaction
    date
    [ post "Assets:Liquid:BCGE" (makeCashAmount $ Cash.negate amount)
        & set pStatus Pending
    , post "Assets:Liquid:Degiro" (makeCashAmount amount)
        & set pStatus Cleared
          . set
            pBalanceAssertion
            (balassert $ makeCashAmount balance)
    ]
    & set tDescription "Deposit"

data ConnectionFee = ConnectionFee
  { _cfDate :: Day
  , _cfAmount :: Cash
  , _cfBalance :: Cash
  }

connectionFeeP :: DegiroCsvRecord -> Maybe ConnectionFee
connectionFeeP rec
  | "DEGIRO Exchange Connection Fee" `T.isInfixOf` dcrDescription rec = do
    change <- dcrChange rec
    return $ ConnectionFee (dcrDate rec) change (dcrBalance rec)
  | otherwise = Nothing

connectionFeeToTransaction :: ConnectionFee -> Transaction
connectionFeeToTransaction (ConnectionFee date amount balance) =
  transaction
    date
    [ post "Assets:Liquid:Degiro" (makeCashAmount amount)
        & set
          pBalanceAssertion
          (balassert $ makeCashAmount balance)
    , post "Expenses:Financial Services" (makeCashAmount $ Cash.negate amount)
    ]
    & set tDescription "Exchange Connection Fee"
      . set tStatus Cleared

data FxType = Credit | Debit

data FxRow = FxRow
  { fxRowDate :: Day
  , fxRowTime :: TimeOfDay
  , fxRowFx :: Maybe Decimal
  , fxRowChange :: Cash
  , fxRowBalance :: Cash
  }

fxTypeP :: Text -> Maybe FxType
fxTypeP "FX Credit" = Just Credit
fxTypeP "FX Debit" = Just Debit
fxTypeP _ = Nothing

fxRowP :: DegiroCsvRecord -> Maybe FxRow
fxRowP rec = do
  void $ fxTypeP $ dcrDescription rec
  change <- dcrChange rec
  return $ FxRow (dcrDate rec) (dcrTime rec) (dcrFx rec) change (dcrBalance rec)

data FxPosting = FxPosting
  { fxPostingFx :: Maybe Decimal
  , fxPostingCurrency :: !Currency
  , _fxPostingChange :: !Decimal
  , _fxPostingBalance :: !Decimal
  }

mkFxPosting :: Maybe Decimal -> Cash -> Cash -> Maybe FxPosting
mkFxPosting maybeFx change balance = do
  guard (((==) `on` view cashCurrency) change balance)
  return $
    FxPosting
      maybeFx
      (view cashCurrency change)
      (view cashAmount change)
      (view cashAmount balance)

fxPostingToPosting :: FxPosting -> Posting
fxPostingToPosting (FxPosting _fx currency change balance) =
  post
    "Assets:Liquid:Degiro"
    ( makeCashAmount (Cash currency change)
    )
    & set pBalanceAssertion (balassert $ makeCashAmount (Cash currency balance))

data Fx = Fx
  { _fxDate :: !Day
  , _fxFstPosting :: !FxPosting
  , _fxSndPosting :: !FxPosting
  }

fxP :: FxRow -> FxRow -> Either Text Fx
fxP fxRowFst fxRowSnd = mapLeft ("Could not merge Fx rows.\n" <>) $
  maybeToRight "" $ do
    guard (((==) `on` fxRowDate) fxRowFst fxRowSnd)
    guard (((==) `on` fxRowTime) fxRowFst fxRowSnd)
    guard (((||) `on` (isJust . fxRowFx)) fxRowFst fxRowSnd)
    fstP <-
      mkFxPosting
        (fxRowFx fxRowFst)
        (fxRowChange fxRowFst)
        (fxRowBalance fxRowFst)
    sndP <-
      mkFxPosting
        (fxRowFx fxRowSnd)
        (fxRowChange fxRowSnd)
        (fxRowBalance fxRowSnd)
    return $ Fx (fxRowDate fxRowFst) fstP sndP

fxToTransaction :: Fx -> Transaction
fxToTransaction (Fx date fstPost sndPost) =
  transaction
    date
    [ fxPostingToPosting fstPost
        & setPrice sndPost
    , fxPostingToPosting sndPost
        & setPrice fstPost
    ]
    & set tDescription "Degiro Forex"
      . set tStatus Cleared
 where
  setPrice postArg =
    set
      (pAmount . aAmountPrice)
      ( UnitPrice . amountSetFullPrecision . makeCashAmount
          . Cash
            (fxPostingCurrency postArg)
          <$> fxPostingFx postArg
      )

data StockTrade = StockTrade
  { _stDate :: !Day
  , _stIsin :: !Isin
  , _stQuantity :: !Int
  , _stPrice :: !Cash
  , _stChange :: !Cash
  , _stBalance :: !Cash
  }

data StockTradeType = Buy | Sell
  deriving stock (Bounded, Enum, Eq, Show)

stockTradeTypeP :: Text -> Maybe StockTradeType
stockTradeTypeP = inverseMap show

data StockTradeDescription = StockTradeDescription
  { _stdType :: !StockTradeType
  , _stdQuantity :: !Int
  , _stdPrice :: !Cash
  }

stockTradeDescriptionP :: Text -> Maybe StockTradeDescription
stockTradeDescriptionP = MP.parseMaybe parserP
 where
  parserP :: Parsec Void Text StockTradeDescription
  parserP = do
    Just tradeType <- stockTradeTypeP . toText <$> some letterChar
    space
    quantity <- decimal
    void $ manyTill anySingle (single '@')
    price <- decimalP defaultDecimalFormat
    space
    currency <- currencyP
    void $ many anySingle
    return $ StockTradeDescription tradeType quantity (Cash currency price)

stockTradeP :: DegiroCsvRecord -> Maybe StockTrade
stockTradeP rec = do
  trIsin <- dcrIsin rec
  (StockTradeDescription trType quantity price) <-
    stockTradeDescriptionP $
      dcrDescription rec
  let qtyChange = case trType of
        Buy -> id
        Sell -> negate
  change <- dcrChange rec
  return $ StockTrade (dcrDate rec) trIsin (qtyChange quantity) price change (dcrBalance rec)

stockTradeToTransaction :: StockTrade -> Transaction
stockTradeToTransaction (StockTrade date trIsin qty price change bal) =
  transaction
    date
    [ post
        ("Assets:Investments:Degiro:" <> prettyStockName)
        ( makeCommodityAmount
            prettyStockName
            (fromRational $ toInteger qty % 1)
            & set
              aAmountPrice
              ( Just
                  . UnitPrice
                  . amountSetFullPrecision
                  . makeCashAmount
                  $ price
              )
        )
    , post "Assets:Liquid:Degiro" (makeCashAmount change)
        & set
          pBalanceAssertion
          (balassert $ makeCashAmount bal)
    ]
    & set tStatus Cleared
      . set tDescription "Degiro Stock Transaction"
 where
  prettyStockName = prettyIsin trIsin

-- | Represents money market records
--
-- Degiro statements provide information on how the cash fares in their money market fund.
-- The changes tend to be pennies, so I want to ignore them.
data MoneyMarketOp = MoneyMarketOp

moneyMarketFundOpP :: DegiroCsvRecord -> Maybe MoneyMarketOp
moneyMarketFundOpP rec = do
  recIsin <- dcrIsin rec
  guard $ recIsin == moneyMarketIsin
  return MoneyMarketOp

moneyMarketFundPriceChangeP :: DegiroCsvRecord -> Maybe MoneyMarketOp
moneyMarketFundPriceChangeP rec = do
  guard $ (== "Money Market fund price change (CHF)") $ dcrDescription rec
  return MoneyMarketOp

data Activity
  = ActivityDeposit Deposit
  | ActivityConnectionFee ConnectionFee
  | ActivityFx Fx
  | ActivityStockTrade StockTrade

class ToActivity a where
  toActivity :: a -> Activity

instance ToActivity Deposit where
  toActivity = ActivityDeposit

instance ToActivity ConnectionFee where
  toActivity = ActivityConnectionFee

instance ToActivity Fx where
  toActivity = ActivityFx

instance ToActivity StockTrade where
  toActivity = ActivityStockTrade

activityToTransaction :: Activity -> Transaction
activityToTransaction (ActivityDeposit dep) = depositToTransaction dep
activityToTransaction (ActivityConnectionFee cf) = connectionFeeToTransaction cf
activityToTransaction (ActivityFx fx) = fxToTransaction fx
activityToTransaction (ActivityStockTrade st) = stockTradeToTransaction st

newtype DegiroCsv = DegiroCsv [DegiroCsvRecord]
  deriving newtype (Stream)

instance VisualStream DegiroCsv where
  showTokens _proxy = show

moneyMarketFundOpParsec :: Parsec ErrorText DegiroCsv MoneyMarketOp
moneyMarketFundOpParsec = label "Money Market Operation" $ token moneyMarketFundOpP S.empty

moneyMarketFundPriceChangeParsec :: Parsec ErrorText DegiroCsv MoneyMarketOp
moneyMarketFundPriceChangeParsec =
  label "Money Market Fund Price Change" $ token moneyMarketFundPriceChangeP S.empty

moneyMarketOpP :: Parsec ErrorText DegiroCsv MoneyMarketOp
moneyMarketOpP = moneyMarketFundPriceChangeParsec <|> moneyMarketFundOpParsec

fxParsec :: Parsec ErrorText DegiroCsv Fx
fxParsec = do
  fstRow <- token fxRowP S.empty
  sndRow <- token fxRowP S.empty <|> customFailure "Found an unmatched fx record."
  case fxP fstRow sndRow of
    Left errMsg -> customFailure $ ErrorText errMsg
    Right res -> return res

activityP :: Parsec ErrorText DegiroCsv Activity
activityP = do
  let singleRecordPs :: [DegiroCsvRecord -> Maybe Activity]
      singleRecordPs =
        [ fmap toActivity . depositP
        , fmap toActivity . withdrawalP
        , fmap toActivity . connectionFeeP
        , fmap toActivity . stockTradeP
        ]
      singleRecordParsecs = (`token` S.empty) <$> singleRecordPs
  choice singleRecordParsecs <|> fmap toActivity fxParsec

activitiesP :: Parsec ErrorText DegiroCsv [Activity]
activitiesP = do
  void $ many moneyMarketOpP
  as <- many (activityP <* many moneyMarketOpP)
  eof
    <|> ( do
            row <- anySingle
            customFailure . ErrorText $
              "Could not process all elements.\n"
                <> "One remaining row's description: "
                <> dcrDescription row
                <> "\n"
        )
  return as

-- | Parses a parsed Degiro CSV statement into stronger types.
csvRecordsToActivities :: [DegiroCsvRecord] -> Either Text [Activity]
csvRecordsToActivities recs =
  mapLeft showAnError $
    parse activitiesP "" (DegiroCsv (reverse recs))
 where
  showAnError :: (VisualStream s, MP.ShowErrorComponent e) => MP.ParseErrorBundle s e -> Text
  showAnError = toText . MP.parseErrorPretty . head . MP.bundleErrors

-- | Transforms a parsed Degiro CSV statement into a Ledger report.
csvRecordsToLedger :: [DegiroCsvRecord] -> Either Text LedgerReport
csvRecordsToLedger recs = do
  activities <- csvRecordsToActivities recs
  return $
    LedgerReport
      { ledgerReportTransactions = activityToTransaction <$> activities
      , ledgerReportMarketPrices = []
      }

-- | Transforms a Degiro CSV statement into a Ledger report.
csvStatementToLedger :: CsvFile LBS.ByteString -> Either Text LedgerReport
csvStatementToLedger stmtTxt = do
  (records :: [DegiroCsvRecord]) <- parseCsvStatement stmtTxt
  csvRecordsToLedger records
