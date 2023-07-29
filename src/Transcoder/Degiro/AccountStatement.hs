-- | This module parses Degiro's account statement into Ledger.
module Transcoder.Degiro.AccountStatement (
  csvStatementToLedger,
  csvRecordsToLedger,
) where

import Control.Lens (set, view)
import Control.Monad.Cont (Cont, MonadCont (callCC), runCont)
import Data.ByteString.Lazy qualified as LBS
import Data.Cash (Cash (Cash), cashAmount, cashCurrency, cashP)
import Data.Cash qualified as Cash
import Data.Decimal (Decimal)
import Data.Either.Combinators (
  mapLeft,
 )
import Data.Ratio ((%))
import Data.Text qualified as T
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
  Comment (NoComment),
  ToPosting (toPosting),
  ToTransaction (toTransaction),
  makeCashAmount,
  makeCommodityAmount,
  makePosting,
  makeTransaction,
 )
import Hledger.Data.Lens (
  aAmountPrice,
  pAmount,
  pBalanceAssertion,
 )
import Relude
import Text.Megaparsec (
  Parsec,
  anySingle,
  manyTill,
  single,
 )
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Transcoder.Data.CsvFile (CsvFile)
import Transcoder.Data.Currency (Currency, currencyP)
import Transcoder.Data.Isin (Isin, isin)
import Transcoder.Data.MyDecimal (
  decimalP,
  defaultDecimalFormat,
 )
import Transcoder.Degiro.Csv (
  DegiroCsvRecord (..),
  DegiroIsin (DegiroIsin, Nlflatexacnt),
  parseCsvStatement,
 )
import Transcoder.Degiro.IsinData (prettyIsin)
import Transcoder.Wallet (
  bcgeAccount,
  degiroAccount,
 )

moneyMarketIsin :: Isin
moneyMarketIsin = [isin|NL0011280581|]

-- | A cash deposit either to or from Degiro.
data Deposit = Deposit
  { depositDate :: !Day
  , depositAmount :: !Cash
  , depositBalance :: !Cash
  -- ^ Final Degiro cash balance.
  }
  deriving stock (Eq, Show)

dcrToDeposit :: DegiroCsvRecord -> Maybe Deposit
dcrToDeposit rec
  | dcrDescription rec `elem` ["Deposit", "Withdrawal"] = do
      change <- dcrChange rec
      return Deposit{depositDate = dcrDate rec, depositAmount = change, depositBalance = dcrBalance rec}
  | otherwise = Nothing

instance ToTransaction Deposit where
  toTransaction (Deposit{depositDate = date, depositAmount = amount, depositBalance = balance}) =
    makeTransaction
      date
      Nothing
      "Deposit"
      [ makePosting (Just Pending) bcgeAccount (Just $ Cash.negate amount) NoComment
      , makePosting (Just Cleared) degiroAccount (Just amount) NoComment
          & set pBalanceAssertion (balassert $ makeCashAmount balance)
      ]

-- | A single stock trade, i.e., a buy or sell transaction without any additional fees etc.
data StockTrade = StockTrade
  { stDate :: !Day
  , stIsin :: !Isin
  , stQuantity :: !Int
  , stPrice :: !Cash
  , stChange :: !Cash
  , stBalance :: !Cash
  }
  deriving stock (Eq, Show)

data StockTradeType = Buy | Sell
  deriving stock (Bounded, Enum, Eq, Show)

stockTradeTypeP :: Text -> Maybe StockTradeType
stockTradeTypeP = inverseMap show

data StockTradeDescription = StockTradeDescription
  { stdType :: !StockTradeType
  , stdQuantity :: !Int
  , stdPrice :: !Cash
  }

stockTradeDescriptionP :: Text -> Maybe StockTradeDescription
stockTradeDescriptionP = MP.parseMaybe parserP
 where
  parserP :: Parsec Void Text StockTradeDescription
  parserP = do
    tradeTypeStr <- toText <$> some letterChar
    tradeType <- maybe (fail "Expected stock trade type") return (stockTradeTypeP tradeTypeStr)
    space
    quantity <- decimal
    void $ manyTill anySingle (single '@')
    price <- decimalP defaultDecimalFormat
    space
    currency <- currencyP
    void $ many anySingle
    return
      StockTradeDescription
        { stdType = tradeType
        , stdQuantity = quantity
        , stdPrice = Cash currency price
        }

dcrToStockTrade :: DegiroCsvRecord -> Maybe StockTrade
dcrToStockTrade rec = do
  trDegiroIsin <- dcrIsin rec
  trIsin <- case trDegiroIsin of
    DegiroIsin is -> return is
    Nlflatexacnt -> Nothing
  (StockTradeDescription trType quantity price) <-
    stockTradeDescriptionP
      $ dcrDescription rec
  let qtyChange = case trType of
        Buy -> id
        Sell -> negate
  change <- dcrChange rec
  return
    StockTrade
      { stDate = dcrDate rec
      , stIsin = trIsin
      , stQuantity = qtyChange quantity
      , stPrice = price
      , stChange = change
      , stBalance = dcrBalance rec
      }

instance ToTransaction StockTrade where
  toTransaction (StockTrade date trIsin qty price change bal) =
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
      , makePosting Nothing degiroAccount (Just change) NoComment
          & set
            pBalanceAssertion
            (balassert $ makeCashAmount bal)
      ]
   where
    prettyStockName = prettyIsin trIsin

data FxType = Credit | Debit

data FxRow = FxRow
  { fxRowDate :: !Day
  , fxRowTime :: !TimeOfDay
  , fxRowFx :: !(Maybe Decimal)
  , fxRowChange :: !Cash
  , fxRowBalance :: !Cash
  }
  deriving stock (Eq, Show)

fxTypeP :: Text -> Maybe FxType
fxTypeP "FX Credit" = Just Credit
fxTypeP "FX Debit" = Just Debit
fxTypeP _ = Nothing

dcrToFxRow :: DegiroCsvRecord -> Maybe FxRow
dcrToFxRow rec = do
  void $ fxTypeP $ dcrDescription rec
  change <- dcrChange rec
  return
    FxRow
      { fxRowDate = dcrDate rec
      , fxRowTime = dcrTime rec
      , fxRowFx = dcrFx rec
      , fxRowChange = change
      , fxRowBalance = dcrBalance rec
      }

-- | A fee paid to or from Degiro.
-- https://www.reddit.com/r/BEFire/comments/q0eil0/degiro_flatex_interestswhat_are_they/
data Fee = Fee
  { fDate :: !Day
  , fDescription :: !Text
  , fAmount :: !Cash
  , fBalance :: !Cash
  }
  deriving stock (Eq, Show)

dcrToFee :: DegiroCsvRecord -> Maybe Fee
dcrToFee rec
  | "Exchange Connection Fee"
      `T.isInfixOf` dcrDescription rec
      || "Flatex Interest"
      `T.isInfixOf` dcrDescription rec
      || "Cash Sweep Transfer"
      `T.isInfixOf` dcrDescription rec = do
      change <- dcrChange rec
      return $ Fee (dcrDate rec) (dcrDescription rec) change (dcrBalance rec)
  | "Transfer from your Cash Account at flatex Bank: " `T.isInfixOf` dcrDescription rec = do
      changeStr <- T.stripPrefix "Transfer from your Cash Account at flatex Bank: " $ dcrDescription rec
      change <- MP.parseMaybe @Void cashP changeStr
      return $ Fee (dcrDate rec) (dcrDescription rec) (Cash.negate change) (dcrBalance rec)
  | otherwise = Nothing

instance ToTransaction Fee where
  toTransaction
    ( Fee
        { fDate = date
        , fDescription = description
        , fAmount = amount
        , fBalance = balance
        }
      ) =
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

-- | A semantic elaboration on DegiroCsvRecord.
--
-- Each instance of this type corresponds to one CSV record but semantically distilled.
data DegiroCsvSmartRecord
  = DCSRDeposit !Deposit
  | DCSRStockTrade !StockTrade
  | DCSRFxRow !FxRow
  | DCSRFee !Fee
  deriving stock (Eq, Show)

csvRecordToSmartRecord :: DegiroCsvRecord -> Maybe DegiroCsvSmartRecord
csvRecordToSmartRecord dcr = flip runCont id $ callCC $ \exit -> do
  let exitOnJust :: Maybe DegiroCsvSmartRecord -> Cont (Maybe DegiroCsvSmartRecord) ()
      exitOnJust = maybe pass (exit . Just)
  exitOnJust $ DCSRDeposit <$> dcrToDeposit dcr
  exitOnJust $ DCSRStockTrade <$> dcrToStockTrade dcr
  exitOnJust $ DCSRFxRow <$> dcrToFxRow dcr
  exitOnJust $ DCSRFee <$> dcrToFee dcr
  return Nothing

data FxPosting = FxPosting
  { fxPostingFx :: !(Maybe Decimal)
  , fxPostingCurrency :: !Currency
  , _fxPostingChange :: !Decimal
  , _fxPostingBalance :: !Decimal
  }

mkFxPosting :: Maybe Decimal -> Cash -> Cash -> Maybe FxPosting
mkFxPosting maybeFx change balance = do
  guard (((==) `on` view cashCurrency) change balance)
  return
    $ FxPosting
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
mergeFx fxRowFst fxRowSnd = mapLeft ("Could not merge Fx rows.\n" <>)
  $ maybeToRight ""
  $ do
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
        ( UnitPrice
            . amountSetFullPrecision
            . makeCashAmount
            . Cash
              (fxPostingCurrency postArg)
            <$> fxPostingFx postArg
        )

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

-- | Activity is any kind of logical transaction on Degiro.
--
-- An activity is represented by one or more CSV records and corresponds to one Ledger transaction.
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
activityToTransaction (ActivityFee cf) = toTransaction cf
activityToTransaction (ActivityFx fx) = toTransaction fx
activityToTransaction (ActivityStockTrade st) = toTransaction st

activityFromFxRows :: FxRow -> FxRow -> Either Text Activity
activityFromFxRows fstRow sndRow = toActivity <$> mergeFx fstRow sndRow

activityFromSmartRecord :: DegiroCsvSmartRecord -> Either Text Activity
activityFromSmartRecord (DCSRDeposit dep) = Right $ toActivity dep
activityFromSmartRecord (DCSRStockTrade st) = Right $ toActivity st
activityFromSmartRecord (DCSRFee cf) = Right $ toActivity cf
activityFromSmartRecord record = Left $ "Can't create an activity. Unrecognized smart record: " <> show record

parseCsvSmartRecords :: [DegiroCsvRecord] -> Either Text [DegiroCsvSmartRecord]
parseCsvSmartRecords dcrs =
  let tryParse :: DegiroCsvRecord -> Either DegiroCsvRecord DegiroCsvSmartRecord
      tryParse dcr = maybe (Left dcr) return $ csvRecordToSmartRecord dcr
      reportError :: DegiroCsvRecord -> Text
      reportError = ("Could not parse CSV record into a smart record: " <>) . dcrDescription
      elems :: [Either Text DegiroCsvSmartRecord]
      elems = mapLeft reportError . tryParse <$> dcrs
   in sequence elems

smartRecordsToActivities :: [DegiroCsvSmartRecord] -> Either Text [Activity]
smartRecordsToActivities ((DCSRFxRow fxx) : ((DCSRFxRow fxy) : ys)) =
  case activityFromFxRows fxx fxy of
    Right activity -> (activity :) <$> smartRecordsToActivities ys
    Left err -> Left err
smartRecordsToActivities (x : xs) = do
  activity <- activityFromSmartRecord x
  rest <- smartRecordsToActivities xs
  return $ activity : rest
smartRecordsToActivities [] = return []

parseActivities :: [DegiroCsvRecord] -> Either Text [Activity]
parseActivities dcrs = parseCsvSmartRecords dcrs >>= smartRecordsToActivities

-- | Parses a parsed Degiro CSV statement into stronger types.
csvRecordsToActivities :: [DegiroCsvRecord] -> Either Text [Activity]
csvRecordsToActivities =
  (parseActivities . reverse)
    . filter (not . isMoneyMarketActivity)

-- | Transforms a parsed Degiro CSV statement into Ledger transactions.
csvRecordsToLedger :: [DegiroCsvRecord] -> Either Text [Transaction]
csvRecordsToLedger recs = activityToTransaction <<$>> csvRecordsToActivities recs

-- | Transforms a Degiro CSV statement into Ledger transactions.
csvStatementToLedger :: CsvFile LBS.ByteString -> Either Text [Transaction]
csvStatementToLedger stmtTxt = do
  (records :: [DegiroCsvRecord]) <- parseCsvStatement stmtTxt
  csvRecordsToLedger records
