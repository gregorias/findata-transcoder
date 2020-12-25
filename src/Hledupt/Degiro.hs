{-# LANGUAGE OverloadedStrings #-}

module Hledupt.Degiro
  ( csvStatementToLedger,
    csvRecordsToLedger,
  )
where

import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as LBS
import Data.List.HT (partitionMaybe)
import qualified Data.Text as Text
import Data.Time (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hledger
  ( Status (Cleared, Pending),
    Transaction,
    balassert,
    post,
    transaction,
  )
import Hledger.Data.Extra (makeCashAmount)
import Hledger.Data.Lens (pBalanceAssertion, pStatus, tDescription, tStatus)
import Hledupt.Data.Cash (Cash)
import qualified Hledupt.Data.Cash as Cash
import Hledupt.Data.LedgerReport (LedgerReport (..))
import Hledupt.Degiro.Csv
  ( DegiroCsvRecord (..),
    Isin,
    mkIsin,
    parseCsvStatement,
  )
import Relude

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

data Activity = ActivityDeposit Deposit | ActivityConnectionFee ConnectionFee

activityToTransaction :: Activity -> Transaction
activityToTransaction (ActivityDeposit dep) = depositToTransaction dep
activityToTransaction (ActivityConnectionFee cf) = connectionFeeToTransaction cf

-- | Filters out money market records
--
-- Degiro statements provide information on how the cash fares in their money market fund.
-- The changes tend to be pennies, so I want to ignore them.
filterOutMoneyMarketRecords :: Isin -> [DegiroCsvRecord] -> [DegiroCsvRecord]
filterOutMoneyMarketRecords mmIsin = filter ((/= pure mmIsin) . dcrIsin)

-- | Parses a parsed Degiro CSV statement into stronger types
csvRecordsToActivities :: [DegiroCsvRecord] -> Either String [Activity]
csvRecordsToActivities recs = do
  Just mmIsin <- return moneyMarketIsin
  (acts, remainingRecs) <-
    return $ runState (transform mmIsin) recs
  case listToMaybe remainingRecs of
    Nothing -> Right acts
    Just row ->
      Left $
        "Hledupt.Degiro.csvRecordsToLedger could not process all elements.\n"
          ++ "One remaining row's description: "
          ++ Text.unpack (dcrDescription row)
          ++ "\n"
  where
    transform :: Isin -> State [DegiroCsvRecord] [Activity]
    transform mmIsin = do
      modify (filterOutMoneyMarketRecords mmIsin)
      (deposits, newRecs0) <- partitionMaybe depositP <$> get
      put newRecs0
      (cfs, newRecs1) <- partitionMaybe connectionFeeP <$> get
      put newRecs1
      return $ (ActivityDeposit <$> deposits) ++ (ActivityConnectionFee <$> cfs)

-- | Transforms a parsed Degiro CSV statement into a Ledger report
csvRecordsToLedger :: Vector DegiroCsvRecord -> Either String LedgerReport
csvRecordsToLedger recs = do
  activities <- csvRecordsToActivities (V.toList recs)
  return $ LedgerReport (activityToTransaction <$> activities) []

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: LBS.ByteString -> Either String LedgerReport
csvStatementToLedger stmtTxt =
  parseCsvStatement stmtTxt
    >>= csvRecordsToLedger
