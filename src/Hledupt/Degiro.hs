module Hledupt.Degiro
  ( csvStatementToLedger,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Hledupt.Degiro.Csv (parseCsvStatement)
import Relude

-- | Transforms a Degiro CSV statement into a Ledger report
csvStatementToLedger :: LBS.ByteString -> Either String Text
csvStatementToLedger stmtTxt = do
  _degiroCsvRecords <- parseCsvStatement stmtTxt
  Left "Unimplemented"
