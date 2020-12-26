{-# LANGUAGE OverloadedStrings #-}

module Hledupt.Degiro
  ( csvStatementToLedger,
    csvRecordsToLedger,
  )
where

import Control.Lens (over, set, view)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.Decimal (Decimal)
import Data.List.HT (partitionMaybe)
import Data.Ratio ((%))
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hledger
  ( AmountPrice (UnitPrice),
    Status (Cleared, Pending),
    Transaction,
    balassert,
    post,
    setFullPrecision,
    transaction,
  )
import Hledger.Data (Posting)
import Hledger.Data.Extra (makeCashAmount, makeCommodityAmount)
import Hledger.Data.Lens
  ( aAmountPrice,
    pAmount,
    pBalanceAssertion,
    pStatus,
    tDescription,
    tStatus,
  )
import Hledupt.Data (decimalParser)
import Hledupt.Data.Cash (Cash (Cash), cashAmount, cashCurrency)
import qualified Hledupt.Data.Cash as Cash
import Hledupt.Data.Currency (Currency, currencyP)
import Hledupt.Data.Isin (Isin, mkIsin)
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Degiro.Csv
  ( DegiroCsvRecord (..),
    parseCsvStatement,
  )
import Relude
import Relude.Extra (inverseMap)
import Text.Megaparsec (Parsec, anySingle, manyTill, single)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)

moneyMarketIsin :: Maybe Isin
moneyMarketIsin = mkIsin "NL0011280581"

data Deposit = Deposit
  { _depositDate :: Day,
    _depositTime :: TimeOfDay,
    _depositAmount :: Cash,
    _depositBalance :: Cash
  }

depositP :: DegiroCsvRecord -> Maybe Deposit
depositP rec
  | dcrDescription rec /= "Deposit" = Nothing
  | otherwise = do
    change <- dcrChange rec
    return $ Deposit (dcrDate rec) (dcrTime rec) change (dcrBalance rec)

depositToTransaction :: Deposit -> Transaction
depositToTransaction (Deposit date _time amount balance) =
  transaction
    date
    [ post "Assets:Liquid:BCGE" (makeCashAmount $ Cash.negate amount)
        & L.set pStatus Pending,
      post "Assets:Liquid:Degiro" (makeCashAmount amount)
        & L.set pStatus Cleared
          . L.set
            pBalanceAssertion
            (balassert $ makeCashAmount balance)
    ]
    & L.set tDescription "Deposit"

data ConnectionFee = ConnectionFee
  { _cfDate :: Day,
    _cfAmount :: Cash,
    _cfBalance :: Cash
  }

connectionFeeP :: DegiroCsvRecord -> Maybe ConnectionFee
connectionFeeP rec
  | "DEGIRO Exchange Connection Fee" `Text.isInfixOf` dcrDescription rec = do
    change <- dcrChange rec
    return $ ConnectionFee (dcrDate rec) change (dcrBalance rec)
  | otherwise = Nothing

connectionFeeToTransaction :: ConnectionFee -> Transaction
connectionFeeToTransaction (ConnectionFee date amount balance) =
  transaction
    date
    [ post "Assets:Liquid:Degiro" (makeCashAmount amount)
        & L.set
          pBalanceAssertion
          (balassert $ makeCashAmount balance),
      post "Expenses:Financial Services" (makeCashAmount $ Cash.negate amount)
    ]
    & L.set tDescription "Exchange Connection Fee"
      . L.set tStatus Cleared

data FxType = Credit | Debit

data FxRow = FxRow
  { fxRowDate :: Day,
    fxRowTime :: TimeOfDay,
    fxRowFx :: Maybe Decimal,
    fxRowChange :: Cash,
    fxRowBalance :: Cash
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
  { fxPostingFx :: Maybe Decimal,
    fxPostingCurrency :: !Currency,
    _fxPostingChange :: !Decimal,
    _fxPostingBalance :: !Decimal
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
    & L.set pBalanceAssertion (balassert $ makeCashAmount (Cash currency balance))

data Fx = Fx
  { _fxDate :: !Day,
    _fxFstPosting :: !FxPosting,
    _fxSndPosting :: !FxPosting
  }

fxP :: [FxRow] -> Either Text [Fx]
fxP = sequenceA . go
  where
    go :: [FxRow] -> [Either Text Fx]
    go [] = []
    go (fxRowFst : fxRowSnd : fxRows) = fx : go fxRows
      where
        fx = over L._Left (Text.append "Could not merge Fx rows.\n") $ do
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
    go [_singleFxRow] = [Left "Found an unmatched fx record."]

fxToTransaction :: Fx -> Transaction
fxToTransaction (Fx date fstPost sndPost) =
  transaction
    date
    [ fxPostingToPosting fstPost
        & setPrice sndPost,
      fxPostingToPosting sndPost
        & setPrice fstPost
    ]
    & L.set tDescription "Degiro Forex"
      . L.set tStatus Cleared
  where
    setPrice postArg =
      L.set
        (pAmount . aAmountPrice)
        ( UnitPrice . setFullPrecision . makeCashAmount
            . Cash
              (fxPostingCurrency postArg)
            <$> fxPostingFx postArg
        )

data StockTrade = StockTrade
  { _stDate :: !Day,
    _stIsin :: !Isin,
    _stQuantity :: !Int,
    _stPrice :: !Cash,
    _stChange :: !Cash,
    _stBalance :: !Cash
  }

data StockTradeType = Buy | Sell
  deriving stock (Bounded, Enum, Eq, Show)

stockTradeTypeP :: Text -> Maybe StockTradeType
stockTradeTypeP = inverseMap (Text.pack . show)

data StockTradeDescription = StockTradeDescription
  { _stdType :: !StockTradeType,
    _stdQuantity :: !Int,
    _stdPrice :: !Cash
  }

stockTradeDescriptionP :: Text -> Maybe StockTradeDescription
stockTradeDescriptionP = MP.parseMaybe parserP
  where
    parserP :: Parsec Void Text StockTradeDescription
    parserP = do
      Just tradeType <- stockTradeTypeP . Text.pack <$> some letterChar
      space
      quantity <- decimal
      void $ manyTill anySingle (single '@')
      price <- decimalParser
      space
      currency <- currencyP
      void $ many anySingle
      return $ StockTradeDescription tradeType quantity (Cash currency price)

stockTradeP :: DegiroCsvRecord -> Maybe StockTrade
stockTradeP rec = do
  isin <- dcrIsin rec
  (StockTradeDescription trType quantity price) <-
    stockTradeDescriptionP $
      dcrDescription rec
  let qtyChange = case trType of
        Buy -> id
        Sell -> negate
  change <- dcrChange rec
  return $ StockTrade (dcrDate rec) isin (qtyChange quantity) price change (dcrBalance rec)

prettyIsin :: Isin -> Text
prettyIsin isin =
  if Just isin == iwda then "IWDA" else show isin
  where
    iwda = mkIsin "IE00B4L5Y983"

stockTradeToTransaction :: StockTrade -> Transaction
stockTradeToTransaction (StockTrade date isin qty price change bal) =
  transaction
    date
    [ post
        ("Assets:Investments:Degiro:" `Text.append` prettyStockName)
        ( makeCommodityAmount
            (Text.unpack prettyStockName)
            (fromRational $ toInteger qty % 1)
            & set
              aAmountPrice
              ( Just
                  . UnitPrice
                  . setFullPrecision
                  . makeCashAmount
                  $ price
              )
        ),
      post "Assets:Liquid:Degiro" (makeCashAmount change)
        & L.set
          pBalanceAssertion
          (balassert $ makeCashAmount bal)
    ]
    & set tStatus Cleared
      . set tDescription "Degiro Stock Transaction"
  where
    prettyStockName = prettyIsin isin

data Activity
  = ActivityDeposit Deposit
  | ActivityConnectionFee ConnectionFee
  | ActivityFx Fx
  | ActivityStockTrade StockTrade

activityToTransaction :: Activity -> Transaction
activityToTransaction (ActivityDeposit dep) = depositToTransaction dep
activityToTransaction (ActivityConnectionFee cf) = connectionFeeToTransaction cf
activityToTransaction (ActivityFx fx) = fxToTransaction fx
activityToTransaction (ActivityStockTrade st) = stockTradeToTransaction st

-- | Filters out money market records
--
-- Degiro statements provide information on how the cash fares in their money market fund.
-- The changes tend to be pennies, so I want to ignore them.
filterOutMoneyMarketRecords :: Isin -> [DegiroCsvRecord] -> [DegiroCsvRecord]
filterOutMoneyMarketRecords mmIsin = filter ((/= pure mmIsin) . dcrIsin)

-- | Parses a parsed Degiro CSV statement into stronger types
csvRecordsToActivities :: [DegiroCsvRecord] -> Either Text [Activity]
csvRecordsToActivities recs = do
  Just mmIsin <- return moneyMarketIsin
  (acts, remainingRecs) <-
    fmap swap . sequence . swap $
      runState (runExceptT (transform mmIsin)) recs
  case listToMaybe remainingRecs of
    Nothing -> Right acts
    Just row ->
      Left $
        "Hledupt.Degiro.csvRecordsToLedger could not process all elements.\n"
          `Text.append` "One remaining row's description: "
          `Text.append` dcrDescription row
          `Text.append` "\n"
  where
    transform :: Isin -> ExceptT Text (State [DegiroCsvRecord]) [Activity]
    transform mmIsin = do
      modify (filterOutMoneyMarketRecords mmIsin)
      (deposits, newRecs0) <- partitionMaybe depositP <$> get
      put newRecs0
      (cfs, newRecs1) <- partitionMaybe connectionFeeP <$> get
      put newRecs1
      (fxRows, newRecs2) <- partitionMaybe fxRowP <$> get
      put newRecs2
      fxs <-
        hoistEither $
          over L._Left ("Could not parse forex transactions.\n" `Text.append`) $
            fxP fxRows
      (sts, newRecs3) <- partitionMaybe stockTradeP <$> get
      put newRecs3
      return $
        (ActivityDeposit <$> deposits)
          ++ (ActivityConnectionFee <$> cfs)
          ++ (ActivityFx <$> fxs)
          ++ (ActivityStockTrade <$> sts)

-- | Transforms a parsed Degiro CSV statement into a Ledger report
csvRecordsToLedger :: Vector DegiroCsvRecord -> Either Text LedgerReport
csvRecordsToLedger recs = do
  activities <- csvRecordsToActivities (V.toList recs)
  return $ LedgerReport (activityToTransaction <$> activities) []

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: LBS.ByteString -> Either Text LedgerReport
csvStatementToLedger stmtTxt =
  over L._Left Text.pack (parseCsvStatement stmtTxt)
    >>= csvRecordsToLedger
