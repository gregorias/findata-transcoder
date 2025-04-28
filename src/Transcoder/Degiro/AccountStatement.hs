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
import Data.Decimal.Extra (
  decimalP,
  defaultDecimalFormat,
 )
import Data.Either.Combinators (
  mapLeft,
 )
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.LocalTime (TimeOfDay)
import Hledger (
  AmountCost (UnitCost),
  Status (Cleared, Pending),
  Transaction,
  amountSetFullPrecision,
  balassert,
  post,
 )
import Hledger.Data.Extra (
  Comment (NoComment),
  ToAmount (..),
  ToPosting (toPosting),
  ToTransaction (toTransaction),
  makeCommodityAmount,
  makePosting,
  makeTransaction,
 )
import Hledger.Data.Lens (
  aAmountCost,
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
      [ makePosting (Just Pending) bcgeAccount (Just . toAmount $ Cash.negate amount) NoComment
      , makePosting (Just Cleared) degiroAccount (Just $ toAmount amount) NoComment
          & set pBalanceAssertion (balassert $ toAmount balance)
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
                aAmountCost
                ( Just
                    . UnitCost
                    . amountSetFullPrecision
                    . toAmount
                    $ price
                )
          )
      , makePosting Nothing degiroAccount (Just $ toAmount change) NoComment
          & set
            pBalanceAssertion
            (balassert $ toAmount bal)
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
      || "DEGIRO Transaction and/or third party fees"
      `T.isInfixOf` dcrDescription rec
      || "Flatex Interest"
      `T.isInfixOf` dcrDescription rec = do
      change <- dcrChange rec
      return $ Fee (dcrDate rec) (dcrDescription rec) change (dcrBalance rec)
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
        [ post degiroAccount (toAmount amount)
            & set
              pBalanceAssertion
              (balassert $ toAmount balance)
        , post "Expenses:Financial Services" (toAmount $ Cash.negate amount)
        ]

-- A transfer between the cash account and the investment account.
data CashAccountTransfer = CashAccountTransfer
  { cashAccountTransferDate :: !Day
  , cashAccountTransferTime :: !TimeOfDay
  , cashAccountTransferDescription :: !Text
  , cashAccountTransferAmount :: !Cash
  , cashAccountTransferBalance :: !Cash
  }
  deriving stock (Eq, Show)

cashAccountTransferFromPrefix :: Text
cashAccountTransferFromPrefix = "Transfer from your Cash Account at flatex Bank: "

cashAccountTransferToPrefix :: Text
cashAccountTransferToPrefix = "Transfer to your Cash Account at flatex Bank: "

dcrToCashAccountTransfer :: DegiroCsvRecord -> Maybe CashAccountTransfer
dcrToCashAccountTransfer rec
  | cashAccountTransferFromPrefix `T.isInfixOf` dcrDescription rec = do
      changeStr <- T.stripPrefix cashAccountTransferFromPrefix $ dcrDescription rec
      change <- MP.parseMaybe @Void cashP changeStr
      return
        CashAccountTransfer
          { cashAccountTransferDate = dcrDate rec
          , cashAccountTransferTime = dcrTime rec
          , cashAccountTransferDescription = dcrDescription rec
          , cashAccountTransferAmount = Cash.negate change
          , cashAccountTransferBalance = dcrBalance rec
          }
  | cashAccountTransferToPrefix `T.isInfixOf` dcrDescription rec = do
      changeStr <- T.stripPrefix cashAccountTransferToPrefix $ dcrDescription rec
      change <- MP.parseMaybe @Void cashP changeStr
      return
        CashAccountTransfer
          { cashAccountTransferDate = dcrDate rec
          , cashAccountTransferTime = dcrTime rec
          , cashAccountTransferDescription = dcrDescription rec
          , cashAccountTransferAmount = change
          , cashAccountTransferBalance = dcrBalance rec
          }
  | otherwise = Nothing

-- | A single CSV row described as a cash sweep transfer.
data CashSweepTransfer = CashSweepTransfer
  { cashSweepTransferDate :: !Day
  , cashSweepTransferTime :: !TimeOfDay
  , cashSweepTransferAmount :: !Cash
  , cashSweepTransferBalance :: !Cash
  }
  deriving stock (Eq, Show)

dcrToCashSweepTransfer :: DegiroCsvRecord -> Maybe CashSweepTransfer
dcrToCashSweepTransfer dcr = do
  guard $ dcrDescription dcr == "Degiro Cash Sweep Transfer"
  change <- dcrChange dcr

  return
    CashSweepTransfer
      { cashSweepTransferDate = dcrDate dcr
      , cashSweepTransferTime = dcrTime dcr
      , cashSweepTransferAmount = change
      , cashSweepTransferBalance = dcrBalance dcr
      }

-- | A semantic elaboration on DegiroCsvRecord.
--
-- Each instance of this type corresponds to one CSV record but semantically distilled.
data DegiroCsvSmartRecord
  = DCSRDeposit !Deposit
  | DCSRStockTrade !StockTrade
  | DCSRFxRow !FxRow
  | DCSRFee !Fee
  | DCSRCashAccountTransfer !CashAccountTransfer
  | DCSRCashSweepTransfer !CashSweepTransfer
  deriving stock (Eq, Show)

csvRecordToSmartRecord :: DegiroCsvRecord -> Maybe DegiroCsvSmartRecord
csvRecordToSmartRecord dcr = flip runCont id $ callCC $ \exit -> do
  let exitOnJust :: Maybe DegiroCsvSmartRecord -> Cont (Maybe DegiroCsvSmartRecord) ()
      exitOnJust = maybe pass (exit . Just)
  exitOnJust $ DCSRDeposit <$> dcrToDeposit dcr
  exitOnJust $ DCSRStockTrade <$> dcrToStockTrade dcr
  exitOnJust $ DCSRFxRow <$> dcrToFxRow dcr
  exitOnJust $ DCSRFee <$> dcrToFee dcr
  exitOnJust $ DCSRCashAccountTransfer <$> dcrToCashAccountTransfer dcr
  exitOnJust $ DCSRCashSweepTransfer <$> dcrToCashSweepTransfer dcr
  return Nothing

-- | A cash sweep activity consists of a CashSweepTransfer and a
-- CashAccountTransfer that nullify each other.
data CashSweepActivity = CashSweepActivity
  { cashSweepActivityDate :: !Day
  , cashSweepActivityTime :: !TimeOfDay
  }

cashSweepActivityFromCashTransfers :: CashSweepTransfer -> CashAccountTransfer -> Either Text CashSweepActivity
cashSweepActivityFromCashTransfers
  CashSweepTransfer
    { cashSweepTransferDate = sweepDate
    , cashSweepTransferTime = sweepTime
    , cashSweepTransferAmount = sweepAmount
    }
  CashAccountTransfer
    { cashAccountTransferDate = accountDate
    , cashAccountTransferTime = accountTime
    , cashAccountTransferAmount = accountAmount
    } = do
    unless (sweepDate == accountDate) $ Left ("Cash sweep dates do not match: " <> show sweepDate <> " vs " <> show accountDate)
    unless (sweepTime == accountTime) $ Left ("Cash sweep times do not match: " <> show sweepTime <> " vs " <> show accountTime)
    unless (sweepAmount == Cash.negate accountAmount) $ Left ("Cash sweep amounts do not match: " <> show sweepAmount <> " vs " <> show accountAmount)
    return CashSweepActivity{cashSweepActivityDate = sweepDate, cashSweepActivityTime = sweepTime}

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
      (toAmount (Cash currency change))
      & set pBalanceAssertion (balassert $ toAmount (Cash currency balance))

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
        (pAmount . aAmountCost)
        ( UnitCost
            . amountSetFullPrecision
            . toAmount
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
  | ActivityCashSweep !CashSweepActivity

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

-- | Converts an activity to a transaction.
--
-- Not every activity results in a transaction. For example, a cash sweep activity is ignored.
activityToTransaction :: Activity -> Maybe Transaction
activityToTransaction (ActivityDeposit dep) = return $ toTransaction dep
activityToTransaction (ActivityFee cf) = return $ toTransaction cf
activityToTransaction (ActivityFx fx) = return $ toTransaction fx
activityToTransaction (ActivityStockTrade st) = return $ toTransaction st
activityToTransaction (ActivityCashSweep _) = Nothing

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
smartRecordsToActivities ((DCSRCashSweepTransfer sweepTransfer) : ((DCSRCashAccountTransfer accountTransfer) : ys)) =
  case cashSweepActivityFromCashTransfers sweepTransfer accountTransfer of
    Right activity -> (ActivityCashSweep activity :) <$> smartRecordsToActivities ys
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
csvRecordsToLedger recs = catMaybes <$> (activityToTransaction <<$>> csvRecordsToActivities recs)

-- | Transforms a Degiro CSV statement into Ledger transactions.
csvStatementToLedger :: CsvFile LBS.ByteString -> Either Text [Transaction]
csvStatementToLedger stmtTxt = do
  (records :: [DegiroCsvRecord]) <- parseCsvStatement stmtTxt
  csvRecordsToLedger records
