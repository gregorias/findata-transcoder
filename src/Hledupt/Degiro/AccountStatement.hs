-- | This module parses Degiro's account statement into Ledger.
module Hledupt.Degiro.AccountStatement (
  csvStatementToLedger,
  csvRecordsToLedger,
) where

import Control.Lens (set, view)
import Control.Monad (msum)
import qualified Data.ByteString.Lazy as LBS
import Data.Decimal (Decimal)
import Data.Either.Combinators (
  mapLeft,
 )
import Data.Ratio ((%))
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
 )
import Hledger.Data.Extra (
  ToPosting (toPosting),
  ToTransaction (toTransaction),
  makeCashAmount,
  makeCommodityAmount,
  makeTransaction,
 )
import Hledger.Data.Lens (
  aAmountPrice,
  pAmount,
  pBalanceAssertion,
  pStatus,
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
  DegiroIsin (DegiroIsin, Nlflatexacnt),
  parseCsvStatement,
 )
import Hledupt.Degiro.IsinData (prettyIsin)
import Hledupt.Wallet (
  bcgeAccount,
  degiroAccount,
 )
import Relude
import Text.Megaparsec (
  Parsec,
  anySingle,
  manyTill,
  single,
 )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)

moneyMarketIsin :: Isin
moneyMarketIsin = [isin|NL0011280581|]

-- | A cash deposit either to or from Degiro.
data Deposit = Deposit
  { depositDate :: !Day
  , depositAmount :: !Cash
  , -- | Final Degiro cash balance.
    depositBalance :: !Cash
  }

dcrToDeposit :: DegiroCsvRecord -> Maybe Deposit
dcrToDeposit rec
  | dcrDescription rec `elem` ["Deposit", "Withdrawal"] = do
    change <- dcrChange rec
    return $ Deposit (dcrDate rec) change (dcrBalance rec)
  | otherwise = Nothing

instance ToTransaction Deposit where
  toTransaction (Deposit{depositDate = date, depositAmount = amount, depositBalance = balance}) =
    makeTransaction
      date
      Nothing
      "Deposit"
      [ post bcgeAccount (makeCashAmount $ Cash.negate amount)
          & set pStatus Pending
      , post degiroAccount (makeCashAmount amount)
          & set pStatus Cleared
            . set
              pBalanceAssertion
              (balassert $ makeCashAmount balance)
      ]

-- | A connection fee paid to Degiro.
-- https://www.reddit.com/r/BEFire/comments/q0eil0/degiro_flatex_interestswhat_are_they/
data Fee = Fee
  { fDate :: !Day
  , fDescription :: !Text
  , fAmount :: !Cash
  , fBalance :: !Cash
  }

dcrToFee :: DegiroCsvRecord -> Maybe Fee
dcrToFee rec
  | "Exchange Connection Fee" `T.isInfixOf` dcrDescription rec
      || "Flatex Interest" `T.isInfixOf` dcrDescription rec = do
    change <- dcrChange rec
    return $ Fee (dcrDate rec) (dcrDescription rec) change (dcrBalance rec)
  | otherwise = Nothing

feeToTransaction :: Fee -> Transaction
feeToTransaction (Fee{fDate = date, fDescription = description, fAmount = amount, fBalance = balance}) =
  makeTransaction
    date
    (Just Cleared)
    description
    [ post degiroAccount (makeCashAmount amount)
        & set
          pBalanceAssertion
          (balassert $ makeCashAmount balance)
    , post "Expenses:Financial Services" (makeCashAmount $ Cash.negate amount)
    ]

data FxType = Credit | Debit

data FxRow = FxRow
  { fxRowDate :: !Day
  , fxRowTime :: !TimeOfDay
  , fxRowFx :: !(Maybe Decimal)
  , fxRowChange :: !Cash
  , fxRowBalance :: !Cash
  }

fxTypeP :: Text -> Maybe FxType
fxTypeP "FX Credit" = Just Credit
fxTypeP "FX Debit" = Just Debit
fxTypeP _ = Nothing

dcrToFxRow :: DegiroCsvRecord -> Maybe FxRow
dcrToFxRow rec = do
  void $ fxTypeP $ dcrDescription rec
  change <- dcrChange rec
  return $ FxRow (dcrDate rec) (dcrTime rec) (dcrFx rec) change (dcrBalance rec)

data FxPosting = FxPosting
  { fxPostingFx :: !(Maybe Decimal)
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

instance ToPosting FxPosting where
  toPosting (FxPosting _fx currency change balance) =
    post
      degiroAccount
      ( makeCashAmount (Cash currency change)
      )
      & set pBalanceAssertion (balassert $ makeCashAmount (Cash currency balance))

data Fx = Fx
  { _fxDate :: !Day
  , _fxFstPosting :: !FxPosting
  , _fxSndPosting :: !FxPosting
  }

mergeFx :: FxRow -> FxRow -> Either Text Fx
mergeFx fxRowFst fxRowSnd = mapLeft ("Could not merge Fx rows.\n" <>) $
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

instance ToTransaction Fx where
  toTransaction :: Fx -> Transaction
  toTransaction (Fx date fstPost sndPost) =
    makeTransaction
      date
      (Just Cleared)
      "Degiro Forex"
      [ toPosting fstPost & setPrice sndPost
      , toPosting sndPost & setPrice fstPost
      ]
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

dcrToStockTrade :: DegiroCsvRecord -> Maybe StockTrade
dcrToStockTrade rec = do
  trDegiroIsin <- dcrIsin rec
  trIsin <- case trDegiroIsin of
    DegiroIsin is -> return is
    Nlflatexacnt -> Nothing
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
  makeTransaction
    date
    (Just Cleared)
    "Degiro Stock Transaction"
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
    , post degiroAccount (makeCashAmount change)
        & set
          pBalanceAssertion
          (balassert $ makeCashAmount bal)
    ]
 where
  prettyStockName = prettyIsin trIsin

isMoneyMarketFundOp :: DegiroCsvRecord -> Bool
isMoneyMarketFundOp = (== Just (DegiroIsin moneyMarketIsin)) . dcrIsin

isMoneyMarketFundPriceChange :: DegiroCsvRecord -> Bool
isMoneyMarketFundPriceChange = (== "Money Market fund price change (CHF)") . dcrDescription

-- | Returns if the record is a money market record
--
-- Degiro statements provide information on how the cash fares in their money market fund.
-- The changes tend to be pennies, so I want to ignore them.
isMoneyMarketActivity :: DegiroCsvRecord -> Bool
isMoneyMarketActivity = or . flap [isMoneyMarketFundOp, isMoneyMarketFundPriceChange]

data Activity
  = ActivityDeposit Deposit
  | ActivityFee Fee
  | ActivityFx Fx
  | ActivityStockTrade StockTrade

class ToActivity a where
  toActivity :: a -> Activity

instance ToActivity Deposit where
  toActivity = ActivityDeposit

instance ToActivity Fee where
  toActivity = ActivityFee

instance ToActivity Fx where
  toActivity = ActivityFx

instance ToActivity StockTrade where
  toActivity = ActivityStockTrade

activityToTransaction :: Activity -> Transaction
activityToTransaction (ActivityDeposit dep) = toTransaction dep
activityToTransaction (ActivityFee cf) = feeToTransaction cf
activityToTransaction (ActivityFx fx) = toTransaction fx
activityToTransaction (ActivityStockTrade st) = stockTradeToTransaction st

dcrsToFx :: DegiroCsvRecord -> DegiroCsvRecord -> Either Text (Maybe Fx)
dcrsToFx fstRec sndRec
  | isNothing (dcrToFxRow fstRec) = return Nothing
  | otherwise =
    do
      fstRow <- case dcrToFxRow fstRec of
        Just r -> return r
        Nothing -> Left . toText @String $ "Could not parse an FX record: " <> show fstRec
      sndRow <- case dcrToFxRow sndRec of
        Just r -> return r
        Nothing -> Left . toText @String $ "Found an unmatched FX record: " <> show fstRec
      Just <$> mergeFx fstRow sndRow

dcrsToFxActivity :: DegiroCsvRecord -> DegiroCsvRecord -> Either Text (Maybe Activity)
dcrsToFxActivity fstRec sndRec = toActivity <<$>> dcrsToFx fstRec sndRec

dcrToSingleRowActivity :: DegiroCsvRecord -> Maybe Activity
dcrToSingleRowActivity rec =
  msum $
    flap
      [ fmap toActivity . dcrToDeposit
      , fmap toActivity . dcrToFee
      , fmap toActivity . dcrToStockTrade
      ]
      rec

parseActivitiesErrorString :: DegiroCsvRecord -> Either Text a
parseActivitiesErrorString row =
  Left $
    "Could not process all elements.\n"
      <> "One remaining row's description: "
      <> dcrDescription row
      <> "\n"

parseActivities :: [DegiroCsvRecord] -> Either Text [Activity]
parseActivities [] = return []
parseActivities [x] = case dcrToSingleRowActivity x of
  Nothing -> parseActivitiesErrorString x
  Just ac -> return [ac]
parseActivities (x : xs@(y : ys)) = case dcrToSingleRowActivity x of
  Nothing -> do
    maybeFx <- dcrsToFxActivity x y
    case maybeFx of
      Nothing -> parseActivitiesErrorString x
      Just ac -> (ac :) <$> parseActivities ys
  Just ac -> (ac :) <$> parseActivities xs

-- | Parses a parsed Degiro CSV statement into stronger types.
csvRecordsToActivities :: [DegiroCsvRecord] -> Either Text [Activity]
csvRecordsToActivities =
  (parseActivities . reverse)
    . filter (not . isMoneyMarketActivity)

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
