module Hledupt.Degiro
  ( csvStatementToLedger,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Hledupt.Degiro.Csv (parseCsvStatement)
import Relude

-- |
csvStatementToLedger :: LBS.ByteString -> Either String Text
csvStatementToLedger stmtTxt = do
  _degiroCsvRecords <- parseCsvStatement stmtTxt
  Left "Unimplemented"
