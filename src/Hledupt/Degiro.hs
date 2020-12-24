module Hledupt.Degiro
  ( csvStatementToLedger,
    csvRecordsToLedger,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
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

-- | Filters out money market records
--
-- Degiro statements provide information on how the cash fares in their money market fund.
-- The changes tend to be pennies, so I want to ignore them.
filterOutMoneyMarketRecords :: Isin -> Vector DegiroCsvRecord -> Vector DegiroCsvRecord
filterOutMoneyMarketRecords mmIsin = V.filter ((/= pure mmIsin) . dcrIsin)

-- | Transforms a parsed Degiro CSV statement into a Ledger report
csvRecordsToLedger :: Vector DegiroCsvRecord -> Either String LedgerReport
csvRecordsToLedger recs = do
  Just mmIsin <- return moneyMarketIsin
  (_, remainingRecs) <-
    return $ runState (transform mmIsin) recs
  case listToMaybe . V.toList $ remainingRecs of
    Nothing -> Right mempty
    Just row ->
      Left $
        "Hledupt.Degiro.csvRecordsToLedger could not process all elements.\n"
          ++ "One remaining row's description: "
          ++ Text.unpack (dcrDescription row)
          ++ "\n"
  where
    transform :: Isin -> State (Vector DegiroCsvRecord) ()
    transform mmIsin = do
      modify (filterOutMoneyMarketRecords mmIsin)

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: LBS.ByteString -> Either String LedgerReport
csvStatementToLedger stmtTxt =
  parseCsvStatement stmtTxt
    >>= csvRecordsToLedger
